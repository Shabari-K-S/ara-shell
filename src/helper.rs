use rustyline::Context;
use rustyline::Helper;
use rustyline::Result;
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use std::borrow::Cow;

use crate::parser::{Lexer, Token};

pub struct AuraHelper {
    pub completer: FilenameCompleter,
}

impl Completer for AuraHelper {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, ctx: &Context<'_>) -> Result<(usize, Vec<Pair>)> {
        self.completer.complete(line, pos, ctx)
    }
}

impl Hinter for AuraHelper {
    type Hint = String;
    fn hint(&self, _line: &str, _pos: usize, _ctx: &Context<'_>) -> Option<String> {
        None
    }
}

impl Validator for AuraHelper {
    fn validate(
        &self,
        _ctx: &mut rustyline::validate::ValidationContext,
    ) -> Result<rustyline::validate::ValidationResult> {
        // self.validator.validate(ctx)
        Ok(rustyline::validate::ValidationResult::Valid(None))
    }

    fn validate_while_typing(&self) -> bool {
        false
    }
}

impl Helper for AuraHelper {}

impl Highlighter for AuraHelper {
    fn highlight<'l, 'p>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        let mut lexer = Lexer::new_highlight(line);
        let mut highlighted = String::new();
        let mut is_command_position = true; // First word is command

        loop {
            match lexer.next_token() {
                Ok(token) => {
                    match token {
                        Token::EOF => break,
                        Token::Word(w) => {
                            if is_command_position {
                                // Green for commands
                                highlighted.push_str("\x1b[32m");
                                highlighted.push_str(&w);
                                highlighted.push_str("\x1b[0m");
                                is_command_position = false;
                            } else {
                                // Default/White for arguments
                                highlighted.push_str(&w);
                            }
                        }
                        Token::Operator(op) => {
                            // Cyan for operators
                            highlighted.push_str("\x1b[36m");
                            highlighted.push_str(&op);
                            highlighted.push_str("\x1b[0m");

                            // Reset command position after pipe/semicolon/and/or
                            if op == "|" || op == ";" || op == "&&" || op == "||" {
                                is_command_position = true;
                            }
                        }
                        Token::IoNumber(n) => {
                            highlighted.push_str(&n.to_string());
                        }
                        Token::Whitespace(ws) => {
                            highlighted.push_str(&ws);
                        }
                        Token::Newline => {
                            highlighted.push('\n');
                        }
                    }
                }
                Err(_) => {
                    return Cow::Borrowed(line);
                }
            }
        }

        Cow::Owned(highlighted)
    }

    fn highlight_char(
        &self,
        _line: &str,
        _pos: usize,
        _kind: rustyline::highlight::CmdKind,
    ) -> bool {
        true
    }
}
