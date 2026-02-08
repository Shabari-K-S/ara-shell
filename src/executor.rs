use crate::ast::{Command, ControlOp, Redirect, RedirectOp};
use crate::builtins::get_builtin;
use crate::state::ShellState;
use nix::sys::wait::{WaitStatus, waitpid};
use nix::unistd::{ForkResult, execvp, fork, pipe};
use std::ffi::CString;
use std::os::fd::{AsRawFd, IntoRawFd, OwnedFd};
use std::process::exit;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("Execution error: {0}")]
    General(String),
    #[error("Nix error: {0}")]
    Nix(#[from] nix::Error),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

pub fn execute(cmd: &Command, state: &mut ShellState) -> Result<i32, ExecError> {
    match cmd {
        Command::Simple {
            args, redirects, ..
        } => execute_simple(args, redirects, state),
        Command::Pipeline(cmds) => execute_pipeline(cmds, state),
        Command::List { left, op, right } => execute_list(left, op, right, state),
        _ => Ok(0), // TODO: Subshell
    }
}

fn execute_simple(
    args: &[String],
    redirects: &[Redirect],
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    if args.is_empty() {
        return Ok(0);
    }

    let program = &args[0];

    // Check for built-ins
    if let Some(builtin) = get_builtin(program) {
        // TODO: Redirects for builtins? (Maybe later)
        match builtin.execute(args, state) {
            Ok(code) => return Ok(code),
            Err(e) => return Err(ExecError::General(e.to_string())),
        }
    }

    // External command
    match unsafe { fork() } {
        Ok(ForkResult::Parent { child, .. }) => {
            match waitpid(child, None) {
                Ok(WaitStatus::Exited(_, code)) => Ok(code),
                Ok(WaitStatus::Signaled(_, _, _)) => Ok(128 + 15), // simplified signal exit
                Err(e) => Err(ExecError::Nix(e)),
                _ => Ok(0),
            }
        }
        Ok(ForkResult::Child) => {
            // Handle redirects
            for redirect in redirects {
                handle_redirect(redirect).unwrap_or_else(|e| {
                    eprintln!("Redirection error: {}", e);
                    exit(1);
                });
            }

            let c_program = CString::new(program.clone()).unwrap();
            let c_args: Vec<CString> = args
                .iter()
                .map(|arg| CString::new(arg.clone()).unwrap())
                .collect();

            let _ = execvp(&c_program, &c_args);
            eprintln!("aura: command not found: {}", program);
            exit(127);
        }
        Err(e) => Err(ExecError::Nix(e)),
    }
}

fn execute_pipeline(cmds: &[Command], _state: &mut ShellState) -> Result<i32, ExecError> {
    let mut input_fd: Option<OwnedFd> = None;
    let mut pids = Vec::new();

    for (i, cmd) in cmds.iter().enumerate() {
        let (read_end, write_end) = if i < cmds.len() - 1 {
            let (r, w) = pipe()?;
            (Some(r), Some(w))
        } else {
            (None, None)
        };

        match unsafe { fork() } {
            Ok(ForkResult::Parent { child, .. }) => {
                pids.push(child);
                // Parent closes the write end immediately
                drop(write_end);

                // Pass ownership of 'read_end' to next iteration
                input_fd = read_end;
            }
            Ok(ForkResult::Child) => {
                // Child specific handling

                // If there is an input_fd from previous step, dup2 it to STDIN
                if let Some(fd) = input_fd {
                    let raw_fd = fd.as_raw_fd();
                    unsafe { libc::dup2(raw_fd, 0) };
                }

                // If there is a write_end for next step, dup2 it to STDOUT
                if let Some(fd) = write_end {
                    let raw_fd = fd.as_raw_fd();
                    unsafe { libc::dup2(raw_fd, 1) };
                }

                // Recursively execute command
                if let Command::Simple {
                    args, redirects: _, ..
                } = cmd
                {
                    if args.is_empty() {
                        exit(0);
                    }

                    // TODO: handle redirects mixed with pipes

                    let program = &args[0];
                    let c_program = CString::new(program.clone()).unwrap();
                    let c_args: Vec<CString> = args
                        .iter()
                        .map(|arg| CString::new(arg.clone()).unwrap())
                        .collect();

                    let _ = execvp(&c_program, &c_args);
                    eprintln!("aura: command not found: {}", program);
                    exit(127);
                } else {
                    exit(1);
                }
            }
            Err(e) => return Err(ExecError::Nix(e)),
        }
    }

    // Wait for all children
    let mut last_exit = 0;
    for pid in pids {
        match waitpid(pid, None) {
            Ok(WaitStatus::Exited(_, code)) => last_exit = code,
            _ => {}
        }
    }

    Ok(last_exit)
}

fn execute_list(
    left: &Command,
    op: &ControlOp,
    right: &Command,
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    let left_code = execute(left, state)?;

    let should_run = match op {
        ControlOp::And => left_code == 0,
        ControlOp::Or => left_code != 0,
        ControlOp::Semi => true,
        ControlOp::Async => true, // TODO: Backgrounding logic
    };

    if should_run {
        execute(right, state)
    } else {
        Ok(left_code)
    }
}

fn handle_redirect(redirect: &Redirect) -> Result<(), ExecError> {
    use std::fs::OpenOptions;

    // Simplified File Opening
    let file = match redirect.op {
        RedirectOp::Output => std::fs::File::create(&redirect.target)?,
        RedirectOp::Input => std::fs::File::open(&redirect.target)?,
        RedirectOp::Append => OpenOptions::new()
            .append(true)
            .create(true)
            .open(&redirect.target)?,
        _ => return Ok(()), // TODO
    };

    let fd = file.into_raw_fd();
    unsafe { libc::dup2(fd, redirect.fd) };
    unsafe { libc::close(fd) };

    Ok(())
}
