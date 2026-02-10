use crate::ast::Command;
use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
use nix::unistd::Pid;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct ShellOptions {
    pub interactive: bool,
    pub restricted: bool,
}

// --- Job Control ---

#[derive(Debug, Clone, PartialEq)]
pub enum JobStatus {
    Running,
    Stopped,
    Done(i32),
}

impl fmt::Display for JobStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
            JobStatus::Done(code) => write!(f, "Done({})", code),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Job {
    pub id: usize,
    pub pgid: Pid,
    pub command: String,
    pub status: JobStatus,
}

// --- Shell State ---

pub struct ShellState {
    // Environment variables (separate from OS env to allow scoping/transactions)
    pub env: HashMap<String, String>,
    // Shell variables (not exported)
    pub vars: HashMap<String, String>,
    // Functions
    pub functions: HashMap<String, Command>,
    // Current working directory cache
    pub cwd: PathBuf,
    // Last exit status ($?)
    pub last_exit_code: i32,
    // Parsing options (e.g., restricted mode, interactive)
    pub options: ShellOptions,
    // Alias map for expansion
    pub aliases: HashMap<String, String>,
    // Job table
    pub jobs: Vec<Job>,
    pub next_job_id: usize,
}

impl ShellState {
    pub fn new() -> Self {
        let env_vars: HashMap<String, String> = env::vars().collect();
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));

        Self {
            env: env_vars,
            vars: HashMap::new(),
            functions: HashMap::new(),
            cwd,
            last_exit_code: 0,
            options: ShellOptions::default(),
            aliases: HashMap::new(),
            jobs: Vec::new(),
            next_job_id: 1,
        }
    }

    pub fn get_env(&self, key: &str) -> Option<&String> {
        self.env.get(key)
    }

    pub fn set_cwd(&mut self, path: PathBuf) {
        self.cwd = path;
    }

    // --- Job helpers ---

    pub fn add_job(&mut self, pgid: Pid, command: String) -> usize {
        let id = self.next_job_id;
        self.next_job_id += 1;
        self.jobs.push(Job {
            id,
            pgid,
            command,
            status: JobStatus::Running,
        });
        id
    }

    pub fn remove_job(&mut self, id: usize) {
        self.jobs.retain(|j| j.id != id);
    }

    pub fn find_job(&self, id: usize) -> Option<&Job> {
        self.jobs.iter().find(|j| j.id == id)
    }

    pub fn find_job_mut(&mut self, id: usize) -> Option<&mut Job> {
        self.jobs.iter_mut().find(|j| j.id == id)
    }

    /// Non-blocking reap of completed/stopped background jobs.
    /// Returns a list of (job_id, old_status) for jobs whose status changed.
    pub fn reap_jobs(&mut self) -> Vec<(usize, String, JobStatus)> {
        let mut notifications = Vec::new();

        for job in self.jobs.iter_mut() {
            if job.status == JobStatus::Running || job.status == JobStatus::Stopped {
                match waitpid(
                    Pid::from_raw(-job.pgid.as_raw()),
                    Some(WaitPidFlag::WNOHANG | WaitPidFlag::WUNTRACED),
                ) {
                    Ok(WaitStatus::Exited(_, code)) => {
                        let old = job.status.clone();
                        job.status = JobStatus::Done(code);
                        if old == JobStatus::Running {
                            notifications.push((
                                job.id,
                                job.command.clone(),
                                JobStatus::Done(code),
                            ));
                        }
                    }
                    Ok(WaitStatus::Signaled(_, sig, _)) => {
                        let code = 128 + sig as i32;
                        job.status = JobStatus::Done(code);
                        notifications.push((job.id, job.command.clone(), JobStatus::Done(code)));
                    }
                    Ok(WaitStatus::Stopped(_, _)) => {
                        if job.status != JobStatus::Stopped {
                            job.status = JobStatus::Stopped;
                            notifications.push((job.id, job.command.clone(), JobStatus::Stopped));
                        }
                    }
                    _ => {} // Still running or no change
                }
            }
        }

        // Remove done jobs after notification
        self.jobs
            .retain(|j| !matches!(j.status, JobStatus::Done(_)));

        notifications
    }
}
