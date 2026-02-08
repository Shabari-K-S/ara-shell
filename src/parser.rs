use crate::ast::{Command, Redirect};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ShellError {
    #[error("Syntax error: {0}")]
    Syntax(String),
    #[error("Command not found: {0}")]
    CommandNotFound(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Expansion error: {0}")]
    Expansion(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Word(String),
    Operator(String), // |, &, ;, <, >
    IoNumber(i32),    // 2>
    Newline,
    Whitespace(String),
    EOF,
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    skip_whitespace: bool,
    preserve_quotes: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            skip_whitespace: true,
            preserve_quotes: false,
        }
    }

    pub fn new_highlight(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            skip_whitespace: false,
            preserve_quotes: true,
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn advance_char(&mut self) -> Option<char> {
        if let Some(c) = self.peek_char() {
            self.pos += c.len_utf8();
            Some(c)
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() && c != '\n' {
                self.advance_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, ShellError> {
        if self.skip_whitespace {
            self.skip_whitespace();
        }

        let c = match self.peek_char() {
            Some(c) => c,
            None => return Ok(Token::EOF),
        };

        if !self.skip_whitespace && c.is_whitespace() && c != '\n' {
            let mut ws = String::new();
            while let Some(wc) = self.peek_char() {
                if wc.is_whitespace() && wc != '\n' {
                    ws.push(self.advance_char().unwrap());
                } else {
                    break;
                }
            }
            return Ok(Token::Whitespace(ws));
        }

        match c {
            '\n' => {
                self.advance_char();
                Ok(Token::Newline)
            }
            ';' | '|' | '&' | '<' | '>' => {
                self.advance_char();
                let op = c.to_string();

                // Check for double operators: ||, &&, >>, <<
                if let Some(next_c) = self.peek_char() {
                    let combined = match (c, next_c) {
                        ('|', '|') => Some("||"),
                        ('&', '&') => Some("&&"),
                        ('>', '>') => Some(">>"),
                        ('<', '<') => Some("<<"),
                        _ => None,
                    };

                    if let Some(combined_op) = combined {
                        self.advance_char();
                        return Ok(Token::Operator(combined_op.to_string()));
                    }
                }
                Ok(Token::Operator(op))
            }
            '#' => {
                // Comment, skip until newline
                while let Some(c) = self.peek_char() {
                    if c == '\n' {
                        break;
                    }
                    self.advance_char();
                }
                self.next_token()
            }
            _ => {
                // Word
                let mut word = String::new();
                while let Some(c) = self.peek_char() {
                    if c.is_whitespace() || ";|&<>".contains(c) {
                        break;
                    }

                    if c == '\'' || c == '"' {
                        let quote = self.advance_char().unwrap();
                        if self.preserve_quotes {
                            word.push(quote);
                        }

                        loop {
                            match self.advance_char() {
                                Some(qc) => {
                                    if qc == quote {
                                        if self.preserve_quotes {
                                            word.push(qc);
                                        }
                                        break;
                                    }
                                    word.push(qc);
                                }
                                None => {
                                    return Err(ShellError::Syntax("Unclosed quote".to_string()));
                                }
                            }
                        }
                    } else {
                        word.push(self.advance_char().unwrap());
                    }
                }
                Ok(Token::Word(word))
            }
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Result<Self, ShellError> {
        let current_token = lexer.next_token()?;
        Ok(Self {
            lexer,
            current_token,
        })
    }

    fn advance(&mut self) -> Result<(), ShellError> {
        self.current_token = self.lexer.next_token()?;
        Ok(())
    }

    pub fn parse(&mut self) -> Result<Option<Command>, ShellError> {
        if let Token::EOF = self.current_token {
            return Ok(None);
        }
        let cmd = self.parse_list()?;
        Ok(Some(cmd))
    }

    fn parse_list(&mut self) -> Result<Command, ShellError> {
        let left = self.parse_pipeline()?;

        match &self.current_token {
            Token::Operator(op) if op == "&&" || op == "||" || op == ";" || op == "&" => {
                let op_enum = match op.as_str() {
                    "&&" => crate::ast::ControlOp::And,
                    "||" => crate::ast::ControlOp::Or,
                    ";" => crate::ast::ControlOp::Semi,
                    "&" => crate::ast::ControlOp::Async,
                    _ => unreachable!(),
                };
                self.advance()?;

                // Allow trailing semicolons/ampersands
                if self.current_token == Token::EOF {
                    return Ok(left);
                }

                // Recursively parse the right side
                // Note: This is right-associative for now, but lists in shell are complex.
                // A better approach for "cmd1; cmd2; cmd3" is iteration or left-associativity.
                // For this phase, let's keep it simple: cmd1 ; (cmd2 ; cmd3)
                // However, standard shell grammar often treats list as: sequence of pipelines separated by separators.

                // If the next token is EOF after a separator, return left.
                // But wait, I already did that check.

                // Implementing a loop for flat list structure might be better for iteration,
                // but AST defines List as binary. Let's stick to recursion.

                // Check if we hit EOF or another closing token (if we had ')' or '}')
                if matches!(self.current_token, Token::EOF) {
                    return Ok(left);
                }

                let right = self.parse_list()?;
                Ok(Command::List {
                    left: Box::new(left),
                    op: op_enum,
                    right: Box::new(right),
                })
            }
            _ => Ok(left),
        }
    }

    fn parse_pipeline(&mut self) -> Result<Command, ShellError> {
        let mut commands = vec![self.parse_simple()?];

        while let Token::Operator(op) = &self.current_token {
            if op == "|" {
                self.advance()?;
                commands.push(self.parse_simple()?);
            } else {
                break;
            }
        }

        if commands.len() == 1 {
            Ok(commands.pop().unwrap())
        } else {
            Ok(Command::Pipeline(commands))
        }
    }

    fn parse_simple(&mut self) -> Result<Command, ShellError> {
        let mut args = Vec::new();
        let mut redirects = Vec::new();

        loop {
            match &self.current_token {
                Token::Word(w) => {
                    args.push(w.clone());
                    self.advance()?;
                }
                Token::Operator(op) if op == "<" || op == ">" || op == ">>" => {
                    let op_enum = match op.as_str() {
                        "<" => crate::ast::RedirectOp::Input,
                        ">" => crate::ast::RedirectOp::Output,
                        ">>" => crate::ast::RedirectOp::Append,
                        _ => unreachable!(),
                    };
                    self.advance()?;
                    if let Token::Word(target) = &self.current_token {
                        redirects.push(Redirect {
                            fd: if op_enum == crate::ast::RedirectOp::Input {
                                0
                            } else {
                                1
                            },
                            op: op_enum,
                            target: target.clone(),
                        });
                        self.advance()?;
                    } else {
                        return Err(ShellError::Syntax(
                            "Expected filename after redirection".to_string(),
                        ));
                    }
                }
                _ => break,
            }
        }

        if args.is_empty() && redirects.is_empty() {
            return Err(ShellError::Syntax(
                "Expected command, found nothing".to_string(),
            ));
        }

        Ok(Command::Simple {
            args,
            redirects,
            assignments: vec![], // TODO
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let input = "ls -la";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().unwrap(), Token::Word("ls".to_string()));
        assert_eq!(lexer.next_token().unwrap(), Token::Word("-la".to_string()));
        assert_eq!(lexer.next_token().unwrap(), Token::EOF);
    }

    #[test]
    fn test_lexer_operators() {
        let input = "ls|grep";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().unwrap(), Token::Word("ls".to_string()));
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Operator("|".to_string())
        );
        assert_eq!(lexer.next_token().unwrap(), Token::Word("grep".to_string()));
    }

    #[test]
    fn test_lexer_quotes() {
        let input = "echo \"hello world\"";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token().unwrap(), Token::Word("echo".to_string()));
        assert_eq!(
            lexer.next_token().unwrap(),
            Token::Word("hello world".to_string())
        );
    }

    #[test]
    fn test_parser_simple() {
        let input = "ls -la";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::Simple { args, .. }) => {
                assert_eq!(args, vec!["ls", "-la"]);
            }
            _ => panic!("Expected simple command"),
        }
    }

    #[test]
    fn test_parser_pipeline() {
        let input = "cat file | grep foo";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::Pipeline(cmds)) => {
                assert_eq!(cmds.len(), 2);
            }
            _ => panic!("Expected pipeline"),
        }
    }

    #[test]
    fn test_parser_list() {
        let input = "cd /tmp && ls";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::List { op, .. }) => {
                assert_eq!(op, crate::ast::ControlOp::And);
            }
            _ => panic!("Expected list"),
        }
    }
}
