use nix::sys::signal::{SaFlags, SigAction, SigHandler, SigSet, Signal, sigaction};

/// Setup signal handlers for the shell process itself.
/// Called once at startup before the REPL loop.
///
/// The shell ignores SIGINT and SIGTSTP so that Ctrl-C and Ctrl-Z
/// only affect foreground child processes, not the shell itself.
pub fn setup_shell_signals() {
    let ignore = SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty());

    unsafe {
        // Ctrl-C: don't kill the shell
        let _ = sigaction(Signal::SIGINT, &ignore);
        // Ctrl-Z: don't stop the shell
        let _ = sigaction(Signal::SIGTSTP, &ignore);
        // Ignore SIGTTOU so tcsetpgrp doesn't stop us
        let _ = sigaction(Signal::SIGTTOU, &ignore);
    }
}

/// Restore default signal handlers in a child process after fork().
/// Called in child before exec() so that the child responds normally
/// to signals like SIGINT (Ctrl-C) and SIGTSTP (Ctrl-Z).
pub fn restore_child_signals() {
    let default = SigAction::new(SigHandler::SigDfl, SaFlags::empty(), SigSet::empty());

    unsafe {
        let _ = sigaction(Signal::SIGINT, &default);
        let _ = sigaction(Signal::SIGTSTP, &default);
        let _ = sigaction(Signal::SIGTTOU, &default);
    }
}
