use crate::state::ShellState;
use std::env;
use std::path::Path;
use std::process;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuiltinError {
    #[error("Builtin error: {0}")]
    General(String),
}

pub trait Builtin {
    fn execute(&self, args: &[String], state: &mut ShellState) -> Result<i32, BuiltinError>;
}

pub struct Cd;

impl Builtin for Cd {
    fn execute(&self, args: &[String], state: &mut ShellState) -> Result<i32, BuiltinError> {
        let path = if args.len() > 1 {
            Path::new(&args[1])
        } else {
            // TODO: Handle cd with no args (go to HOME) better
            if let Some(home) = state.env.get("HOME") {
                Path::new(home)
            } else {
                return Err(BuiltinError::General("HOME not set".to_string()));
            }
        };

        match env::set_current_dir(path) {
            Ok(_) => {
                if let Ok(cwd) = env::current_dir() {
                    state.set_cwd(cwd);
                }
                Ok(0)
            }
            Err(e) => {
                eprintln!("cd: {}: {}", args.get(1).unwrap_or(&String::new()), e);
                Ok(1)
            }
        }
    }
}

pub struct Exit;

impl Builtin for Exit {
    fn execute(&self, args: &[String], _state: &mut ShellState) -> Result<i32, BuiltinError> {
        let exit_code = if args.len() > 1 {
            args[1].parse::<i32>().unwrap_or(0)
        } else {
            0
        };
        process::exit(exit_code);
    }
}

pub fn get_builtin(name: &str) -> Option<Box<dyn Builtin>> {
    match name {
        "cd" => Some(Box::new(Cd)),
        "exit" => Some(Box::new(Exit)),
        _ => None,
    }
}
