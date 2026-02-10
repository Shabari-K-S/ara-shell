use ara::{AuraHelper, Lexer, Parser, ShellState, execute, setup_shell_signals};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use rustyline::history::FileHistory;
use std::env;

fn main() -> anyhow::Result<()> {
    // Initialize shell state
    let mut state = ShellState::new();

    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        // Run script
        let path = &args[1];
        let content = std::fs::read_to_string(path)
            .map_err(|e| anyhow::anyhow!("Failed to read script {}: {}", path, e))?;

        let lexer = Lexer::new(&content);
        let mut parser = match Parser::new(lexer) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("Error creating parser: {}", e);
                std::process::exit(1);
            }
        };

        loop {
            match parser.parse() {
                Ok(Some(command)) => {
                    match execute(&command, &mut state) {
                        Ok(code) => {
                            state.last_exit_code = code;
                        }
                        Err(e) => {
                            eprintln!("Error executing command: {}", e);
                            // In scripts, some errors might abort? For now continue.
                        }
                    }
                }
                Ok(None) => break, // EOF
                Err(e) => {
                    eprintln!("Error parsing script: {}", e);
                    std::process::exit(1);
                }
            }
        }

        // Return last exit code
        if state.last_exit_code != 0 {
            std::process::exit(state.last_exit_code);
        }
        return Ok(());
    }

    // Setup signal handling for interactive mode
    setup_shell_signals();
    state.options.interactive = true;

    // Initialize Rustyline Editor with AuraHelper and Ctrl+R history search
    let config = rustyline::config::Config::builder()
        .edit_mode(rustyline::config::EditMode::Emacs)
        .auto_add_history(false) // We add manually for control
        .build();
    let mut rl = Editor::<AuraHelper, FileHistory>::with_config(config)?;
    let completer = rustyline::completion::FilenameCompleter::new();
    rl.set_helper(Some(AuraHelper { completer }));

    // Load history
    // Get home directory for history file
    let history_path = env::var("HOME").unwrap_or_else(|_| ".".to_string()) + "/.aura_history";
    if rl.load_history(&history_path).is_err() {
        // No previous history
    }

    loop {
        // Reap completed background jobs and print notifications
        let notifications = state.reap_jobs();
        for (id, cmd, status) in &notifications {
            eprintln!("[{}]+ {}  {}", id, status, cmd);
        }

        // Display prompt
        let cwd = state.cwd.file_name().unwrap_or_default().to_string_lossy();
        let prompt = format!("\x1b[1;32maura\x1b[0m \x1b[1;36m{}\x1b[0m $ ", cwd);

        let readline = rl.readline(&prompt);
        match readline {
            Ok(line) => {
                let command_text = line.trim();
                if command_text.is_empty() {
                    continue;
                }

                rl.add_history_entry(command_text)?;

                // Lex and Parse
                let lexer = Lexer::new(command_text);
                let parser_result = Parser::new(lexer).and_then(|mut p| p.parse());

                match parser_result {
                    Ok(Some(command)) => {
                        // Execute
                        match execute(&command, &mut state) {
                            Ok(code) => {
                                state.last_exit_code = code;
                            }
                            Err(e) => {
                                eprintln!("Error executing command: {}", e);
                            }
                        }
                    }
                    Ok(None) => {}
                    Err(e) => {
                        eprintln!("{}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                // Ctrl-D
                println!("exit");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history
    rl.save_history(&history_path)?;

    Ok(())
}
