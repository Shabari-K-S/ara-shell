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

pub struct Export;

impl Builtin for Export {
    fn execute(&self, args: &[String], state: &mut ShellState) -> Result<i32, BuiltinError> {
        if args.len() < 2 {
            // Display all environment variables
            for (key, value) in &state.env {
                println!("{}={}", key, value);
            }
            return Ok(0);
        }

        for arg in &args[1..] {
            if let Some((key, value)) = arg.split_once('=') {
                if key.is_empty() {
                    eprintln!("export: `=': not a valid identifier");
                    continue; // bash behavior: invalid identifier, but keep processing others? actually bash stops. let's just continue and error.
                }
                state.env.insert(key.to_string(), value.to_string());
            } else {
                // Behavior for just `export VAR`: ensure it's in env if it was in vars found?
                // For simplified shell, we treat everything as env vars for now in state.env
                // But if we had local vars, we'd move it.
                // If it's not present, we do nothing or create empty?
                // Bash behavior: if VAR is set, mark for export. If not set, nothing visible happens until set.
                // For now, let's just ignore if no = is present, or maybe error?
                // Let's assume simplest: export KEY=VALUE
            }
        }
        Ok(0)
    }
}

pub struct Unset;

impl Builtin for Unset {
    fn execute(&self, args: &[String], state: &mut ShellState) -> Result<i32, BuiltinError> {
        for arg in &args[1..] {
            state.env.remove(arg);
            state.vars.remove(arg);
        }
        Ok(0)
    }
}

pub fn get_builtin(name: &str) -> Option<Box<dyn Builtin>> {
    match name {
        "cd" => Some(Box::new(Cd)),
        "exit" => Some(Box::new(Exit)),
        "export" => Some(Box::new(Export)),
        "unset" => Some(Box::new(Unset)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::state::ShellState;

    #[test]
    fn test_export() {
        let mut state = ShellState::new();
        let export = Export;
        let args = vec!["export".to_string(), "KEY=value".to_string()];

        assert!(export.execute(&args, &mut state).is_ok());
        assert_eq!(state.env.get("KEY").unwrap(), "value");
    }

    #[test]
    fn test_unset() {
        let mut state = ShellState::new();
        state.env.insert("KEY".to_string(), "value".to_string());
        let unset = Unset;
        let args = vec!["unset".to_string(), "KEY".to_string()];

        assert!(unset.execute(&args, &mut state).is_ok());
        assert!(state.env.get("KEY").is_none());
    }
}
