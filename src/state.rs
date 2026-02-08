use std::collections::HashMap;
use std::env;
use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct ShellOptions {
    pub interactive: bool,
    pub restricted: bool,
}

pub struct ShellState {
    // Environment variables (separate from OS env to allow scoping/transactions)
    pub env: HashMap<String, String>,
    // Shell variables (not exported)
    pub vars: HashMap<String, String>,
    // Current working directory cache
    pub cwd: PathBuf,
    // Last exit status ($?)
    pub last_exit_code: i32,
    // Parsing options (e.g., restricted mode, interactive)
    pub options: ShellOptions,
    // Alias map for expansion
    pub aliases: HashMap<String, String>,
}

impl ShellState {
    pub fn new() -> Self {
        let env_vars: HashMap<String, String> = env::vars().collect();
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));

        Self {
            env: env_vars,
            vars: HashMap::new(),
            cwd,
            last_exit_code: 0,
            options: ShellOptions::default(),
            aliases: HashMap::new(),
        }
    }

    pub fn get_env(&self, key: &str) -> Option<&String> {
        self.env.get(key)
    }

    pub fn set_cwd(&mut self, path: PathBuf) {
        self.cwd = path;
    }
}
