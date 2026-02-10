use crate::ast::{Command, ControlOp, Redirect, RedirectOp};
use crate::builtins::get_builtin;
use crate::signals::restore_child_signals;
use crate::state::ShellState;
use nix::sys::wait::{WaitStatus, waitpid};
use nix::unistd::{ForkResult, Pid, execvp, fork, pipe, setpgid};
use std::ffi::CString;
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd, OwnedFd};
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
        Command::If {
            condition,
            then_body,
            else_body,
        } => execute_if(condition, then_body, else_body, state),
        Command::While { condition, body } => execute_while(condition, body, state),
        Command::For {
            variable,
            items,
            body,
        } => execute_for(variable, items, body, state),
        Command::FunctionDef { name, body } => execute_function_def(name, body, state),
        _ => Ok(0), // TODO: Subshell
    }
}

fn execute_function_def(
    name: &str,
    body: &Command,
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    state.functions.insert(name.to_string(), body.clone());
    Ok(0)
}

fn execute_if(
    condition: &Command,
    then_body: &Command,
    else_body: &Option<Box<Command>>,
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    let cond_status = execute(condition, state)?;
    if cond_status == 0 {
        execute(then_body, state)
    } else if let Some(else_cmd) = else_body {
        execute(else_cmd, state)
    } else {
        Ok(0)
    }
}

fn execute_while(
    condition: &Command,
    body: &Command,
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    let mut last_status = 0;
    loop {
        let cond_status = execute(condition, state)?;
        if cond_status == 0 {
            last_status = execute(body, state)?;
        } else {
            break;
        }
    }
    Ok(last_status)
}

fn execute_for(
    variable: &str,
    items: &[String],
    body: &Command,
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    let mut last_status = 0;
    // Todo: expand items here if needed (e.g. $LIST -> "a", "b", "c")
    // For now, assuming items are already split or we don't support splitting yet.
    // But we SHOULD expand variables.

    // Expand items
    let mut expanded_items = Vec::new();
    for item in items {
        // Simple expansion: expand_arg returns one string.
        // Standard shell would split this string by IFS.
        // We skip splitting for now.
        expanded_items.push(expand_arg(item, state));
    }

    for item in expanded_items {
        state.env.insert(variable.to_string(), item);
        last_status = execute(body, state)?;
    }
    Ok(last_status)
}

fn execute_simple(
    args: &[crate::ast::Arg],
    redirects: &[Redirect],
    state: &mut ShellState,
) -> Result<i32, ExecError> {
    if args.is_empty() {
        return Ok(0);
    }

    let mut expanded_args = Vec::new();
    let mut proc_sub_fds = Vec::new(); // Keep FDs open until command finishes
    let mut proc_sub_pids = Vec::new(); // Subprocesses to wait for

    // Expand arguments and setup process substitutions
    for arg in args {
        match arg {
            crate::ast::Arg::Literal(s) => {
                expanded_args.push(expand_arg(s, state));
            }
            crate::ast::Arg::ProcessSub { cmd, direction } => {
                let (read_end, write_end) = pipe()?;
                match unsafe { fork() } {
                    Ok(ForkResult::Parent { child, .. }) => {
                        proc_sub_pids.push(child);

                        match direction {
                            crate::ast::ProcessSubKind::Read => {
                                // <(cmd): cmd writes to pipe, parent reads from pipe
                                // Parent (here) acts as consumer (reading)
                                // We keep read_end open, close write_end
                                drop(write_end);

                                let fd = read_end.as_raw_fd();
                                // /dev/fd/N is safer and standard on Linux
                                expanded_args.push(format!("/dev/fd/{}", fd));
                                proc_sub_fds.push(read_end);
                            }
                            crate::ast::ProcessSubKind::Write => {
                                // >(cmd): cmd reads from pipe, parent writes to pipe
                                // Parent (here) acts as producer (writing)
                                // We keep write_end open, close read_end
                                drop(read_end);

                                let fd = write_end.as_raw_fd();
                                expanded_args.push(format!("/dev/fd/{}", fd));
                                proc_sub_fds.push(write_end);
                            }
                        }
                    }
                    Ok(ForkResult::Child) => {
                        // Child executing the subcommand in the substitution
                        match direction {
                            crate::ast::ProcessSubKind::Read => {
                                // Child writes to stdout
                                drop(read_end);
                                let fd = write_end.into_raw_fd();
                                unsafe { libc::dup2(fd, 1) };
                                unsafe { libc::close(fd) };
                            }
                            crate::ast::ProcessSubKind::Write => {
                                // Child reads from stdin
                                drop(write_end);
                                let fd = read_end.into_raw_fd();
                                unsafe { libc::dup2(fd, 0) };
                                unsafe { libc::close(fd) };
                            }
                        }

                        // Execute the subcommand
                        match execute(cmd, state) {
                            Ok(_) => exit(0),
                            Err(_) => exit(1),
                        }
                    }
                    Err(e) => return Err(ExecError::Nix(e)),
                }
            }
        }
    }

    let program = &expanded_args[0];

    // Helper to cleanup process substitutions
    let mut cleanup = || {
        // Drop the FDs to close them
        proc_sub_fds.clear();
        // Wait for substitution processes
        for pid in &proc_sub_pids {
            let _ = waitpid(*pid, None);
        }
    };

    // Check for functions
    if let Some(body) = state.functions.get(program).cloned() {
        let result = execute(&body, state);
        cleanup();
        return match result {
            Ok(code) => Ok(code),
            Err(e) => Err(ExecError::General(e.to_string())),
        };
    }

    // Check for built-ins
    if let Some(builtin) = get_builtin(program) {
        let result = builtin.execute(&expanded_args, state);
        cleanup();
        return match result {
            Ok(code) => Ok(code),
            Err(e) => Err(ExecError::General(e.to_string())),
        };
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
            // Restore default signal handlers for child
            restore_child_signals();

            // Handle redirects
            for redirect in redirects {
                handle_redirect(redirect).unwrap_or_else(|e| {
                    eprintln!("Redirection error: {}", e);
                    exit(1);
                });
            }

            let c_program = CString::new(program.clone()).unwrap();
            let c_args: Vec<CString> = expanded_args
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

fn expand_arg(arg: &str, state: &ShellState) -> String {
    let mut result = String::new();
    let mut chars = arg.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '$' {
            if let Some(&next) = chars.peek() {
                if next == '?' {
                    chars.next();
                    result.push_str(&state.last_exit_code.to_string());
                } else if next == '{' {
                    chars.next(); // consume '{'
                    let mut var_name = String::new();
                    let mut closed = false;
                    while let Some(c) = chars.next() {
                        if c == '}' {
                            closed = true;
                            break;
                        }
                        var_name.push(c);
                    }
                    if closed {
                        if let Some(val) = state.get_env(&var_name) {
                            result.push_str(val);
                        } else if let Some(val) = state.vars.get(&var_name) {
                            result.push_str(val);
                        }
                    } else {
                        // Malformed ${... , treats as literal for now or error?
                        // Bash behavior checks validity.
                        // Let's just push what we have so far for simplicity/debug
                        result.push_str("${");
                        result.push_str(&var_name);
                    }
                } else if next.is_alphabetic() || next == '_' {
                    // $VAR case
                    let mut var_name = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() || c == '_' {
                            var_name.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    if let Some(val) = state.get_env(&var_name) {
                        result.push_str(val);
                    } else if let Some(val) = state.vars.get(&var_name) {
                        result.push_str(val);
                    }
                } else {
                    // Just a $ followed by something else (e.g. whitespace, or nothing)
                    result.push('$');
                }
            } else {
                // Trailing $
                result.push('$');
            }
        } else {
            result.push(c);
        }
    }
    result
}

fn execute_pipeline(cmds: &[Command], state: &mut ShellState) -> Result<i32, ExecError> {
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
                // Restore default signal handlers for child
                restore_child_signals();

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
                match execute(cmd, state) {
                    Ok(code) => exit(code),
                    Err(e) => {
                        eprintln!("Pipeline execution error: {}", e);
                        exit(1);
                    }
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
    if *op == ControlOp::Async {
        // Background the left command: fork, don't wait, register as job
        // Build a display string for the command
        let cmd_display = format!("{}", left);

        match unsafe { fork() } {
            Ok(ForkResult::Parent { child, .. }) => {
                // Put child in its own process group
                let _ = setpgid(child, child);
                let job_id = state.add_job(child, cmd_display);
                eprintln!("[{}] {}", job_id, child);

                // Now execute the right side (if any)
                return execute(right, state);
            }
            Ok(ForkResult::Child) => {
                // Child: put self in own process group
                let _ = setpgid(Pid::from_raw(0), Pid::from_raw(0));
                restore_child_signals();

                match execute(left, state) {
                    Ok(code) => exit(code),
                    Err(_) => exit(1),
                }
            }
            Err(e) => return Err(ExecError::Nix(e)),
        }
    }

    let left_code = execute(left, state)?;

    let should_run = match op {
        ControlOp::And => left_code == 0,
        ControlOp::Or => left_code != 0,
        ControlOp::Semi => true,
        ControlOp::Async => unreachable!(), // handled above
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
    match redirect.op {
        RedirectOp::Output => {
            let file = std::fs::File::create(&redirect.target)?;
            let fd = file.into_raw_fd();
            unsafe { libc::dup2(fd, redirect.fd) };
            unsafe { libc::close(fd) };
        }
        RedirectOp::Input => {
            let file = std::fs::File::open(&redirect.target)?;
            let fd = file.into_raw_fd();
            unsafe { libc::dup2(fd, redirect.fd) };
            unsafe { libc::close(fd) };
        }
        RedirectOp::Append => {
            let file = OpenOptions::new()
                .append(true)
                .create(true)
                .open(&redirect.target)?;
            let fd = file.into_raw_fd();
            unsafe { libc::dup2(fd, redirect.fd) };
            unsafe { libc::close(fd) };
        }
        RedirectOp::DupOut | RedirectOp::DupIn => {
            // target is FD number, e.g. "1" or "2", or "-" to close
            if redirect.target == "-" {
                unsafe { libc::close(redirect.fd) };
            } else if let Ok(target_fd) = redirect.target.parse::<i32>() {
                unsafe { libc::dup2(target_fd, redirect.fd) };
            } else {
                return Err(ExecError::General(format!(
                    "Invalid FD for duplication: {}",
                    redirect.target
                )));
            }
        }
        RedirectOp::AndOutput => {
            // &> file  ==> > file 2>&1
            // First open file and dup to stdout (1)
            let file = std::fs::File::create(&redirect.target)?;
            let fd = file.into_raw_fd();
            unsafe { libc::dup2(fd, 1) };
            unsafe { libc::close(fd) };
            // Then dup stdout (1) to stderr (2)
            unsafe { libc::dup2(1, 2) };
        }
        RedirectOp::HereString => {
            // Create a pipe
            let (read_end, write_end) = pipe()?;

            // Write string to pipe
            // Need to write in a way that doesn't block if buffer is full?
            // For small strings it's fine. For large ones, might need thread.
            // Using a simple write for now.
            use std::io::Write;
            let mut file = unsafe { std::fs::File::from_raw_fd(write_end.into_raw_fd()) };
            write!(file, "{}\n", redirect.target)?; // Here-strings usually append newline
            drop(file); // closes write end

            let read_fd = read_end.into_raw_fd();
            unsafe { libc::dup2(read_fd, redirect.fd) }; // usually 0
            unsafe { libc::close(read_fd) };
        }
        RedirectOp::HereDoc => {
            // Same as HereString but target contains the multiline content
            // Parser should have already collected the content into 'target'
            // (If we implemented inline parsing properly)
            // For now assuming 'target' IS the content.
            let (read_end, write_end) = pipe()?;
            use std::io::Write;
            let mut file = unsafe { std::fs::File::from_raw_fd(write_end.into_raw_fd()) };
            write!(file, "{}", redirect.target)?;
            drop(file);

            let read_fd = read_end.into_raw_fd();
            unsafe { libc::dup2(read_fd, redirect.fd) };
            unsafe { libc::close(read_fd) };
        }
    };

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::ShellState;

    #[test]
    fn test_expand_arg_basic() {
        let mut state = ShellState::new();
        state.env.insert("VAR".to_string(), "hello".to_string());

        assert_eq!(expand_arg("echo", &state), "echo");
        assert_eq!(expand_arg("$VAR", &state), "hello");
        assert_eq!(expand_arg("${VAR}", &state), "hello");
        assert_eq!(expand_arg("prefix$VAR", &state), "prefixhello");
        assert_eq!(expand_arg("${VAR}suffix", &state), "hellosuffix");
    }

    #[test]
    fn test_expand_arg_missing() {
        let state = ShellState::new();
        assert_eq!(expand_arg("$MISSING", &state), "");
        assert_eq!(expand_arg("${MISSING}", &state), "");
    }

    #[test]
    fn test_expand_arg_special() {
        let mut state = ShellState::new();
        state.last_exit_code = 123;
        assert_eq!(expand_arg("$?", &state), "123");
    }

    #[test]
    fn test_expand_arg_multiple() {
        let mut state = ShellState::new();
        state.env.insert("A".to_string(), "1".to_string());
        state.env.insert("B".to_string(), "2".to_string());
        assert_eq!(expand_arg("$A-$B", &state), "1-2");
    }

    // Mock execution tests would be ideal here, but 'execute' forks.
    // We can't easily test 'execute' logic in unit tests without extensive mocking or refactoring 'execute' to rely on a trait for system calls.
    // However, we can test 'expand_arg' and verifying parser AST is correct (already done in parser.rs).
    // For now, manual verification is key, or integration tests running the binary.
}
