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
            ';' | '|' | '&' | '<' | '>' | '(' | ')' => {
                self.advance_char();
                let op = c.to_string();

                // Check for double/triple operators: ||, &&, >>, <<, <<<, >&, <&, &>
                if let Some(next_c) = self.peek_char() {
                    let mut combined = match (c, next_c) {
                        ('|', '|') => Some("||".to_string()),
                        ('&', '&') => Some("&&".to_string()),
                        ('>', '>') => Some(">>".to_string()),
                        ('<', '<') => Some("<<".to_string()), // Start of << or <<<
                        ('>', '&') => Some(">&".to_string()),
                        ('<', '&') => Some("<&".to_string()),
                        ('&', '>') => Some("&>".to_string()),
                        _ => None,
                    };

                    if let Some(ref val) = combined {
                        self.advance_char();

                        // Check for triple operator <<<
                        if val == "<<" {
                            if let Some(third_c) = self.peek_char() {
                                if third_c == '<' {
                                    self.advance_char();
                                    combined = Some("<<<".to_string());
                                }
                            }
                        }

                        return Ok(Token::Operator(combined.unwrap()));
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
            '0'..='9' => {
                // Check if it's IoNumber (digit followed immediately by < or >)
                // We need to look ahead to see if it's just digits then op, or mixed word.
                // But simplified:
                // 1. Consume digits.
                // 2. Peek next. If < or >, valid IoNumber.
                // 3. If other word char, treat as word.

                let mut num_str = String::new();

                // We know first char is digit
                while let Some(pc) = self.peek_char() {
                    if pc.is_digit(10) {
                        num_str.push(self.advance_char().unwrap());
                    } else {
                        break;
                    }
                }

                // Now peek next
                if let Some(next_c) = self.peek_char() {
                    if next_c == '<' || next_c == '>' {
                        // It IS an IoNumber!
                        // e.g. "2>"
                        return Ok(Token::IoNumber(num_str.parse().unwrap()));
                    } else if next_c.is_whitespace() || ";|&".contains(next_c) || next_c == ')' {
                        // Just a number word: "echo 123"
                        return Ok(Token::Word(num_str));
                    }
                } else {
                    // EOF after number
                    return Ok(Token::Word(num_str));
                }

                // If we are here, it means we hit something else, e.g. "123a"
                // Fallback to word parsing.
                // Reset state? We consumed digits.
                // Continue parsing as word.
                let mut word = num_str;
                while let Some(c) = self.peek_char() {
                    if c.is_whitespace() || ";|&<>()".contains(c) {
                        break;
                    }
                    if c == '\'' || c == '"' {
                        // Quotes in word starting with digit
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
                                    return Err(ShellError::Syntax(
                                        "Unterminated quote".to_string(),
                                    ));
                                }
                            }
                        }
                    } else {
                        word.push(self.advance_char().unwrap());
                    }
                }
                Ok(Token::Word(word))
            }
            _ => {
                // Word
                let mut word = String::new();
                while let Some(c) = self.peek_char() {
                    if c.is_whitespace() || ";|&<>()".contains(c) {
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

    pub fn read_heredoc(&mut self, delimiter: &str) -> String {
        let mut content = String::new();

        // Skip current line logic (advance until newline)
        // Only if we haven't consumed it yet?
        // We assume we are called when parser is at Newline token.
        // So Lexer pos is at start of next line.
        // However, if there is trailing whitespace? Lexer skips whitespace in next_token.
        // But read_heredoc operates on raw input.

        loop {
            if self.pos >= self.input.len() {
                break;
            }

            let remaining = &self.input[self.pos..];
            let end_idx = remaining
                .find('\n')
                .map(|i| i + self.pos)
                .unwrap_or(self.input.len());
            let line = &self.input[self.pos..end_idx];

            if line == delimiter {
                self.pos = end_idx + 1;
                break;
            }

            content.push_str(line);
            content.push('\n');
            self.pos = end_idx + 1;
        }

        content
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
        // Skip leading newlines
        while matches!(self.current_token, Token::Newline) {
            self.advance()?;
        }

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

                // Check for terminators or EOF
                if matches!(self.current_token, Token::EOF) || self.is_terminator() {
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

    fn is_terminator(&self) -> bool {
        match &self.current_token {
            Token::Word(w) => matches!(w.as_str(), "then" | "else" | "fi" | "do" | "done" | "}"),
            _ => false,
        }
    }

    fn parse_pipeline(&mut self) -> Result<Command, ShellError> {
        let mut commands = vec![self.parse_command()?];

        while let Token::Operator(op) = &self.current_token {
            if op == "|" {
                self.advance()?;
                commands.push(self.parse_command()?);
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

    fn parse_command(&mut self) -> Result<Command, ShellError> {
        match &self.current_token {
            Token::Word(w) if w == "if" => self.parse_if(),
            Token::Word(w) if w == "while" => self.parse_while(),
            Token::Word(w) if w == "for" => self.parse_for(),
            Token::Word(w) if w == "{" => self.parse_group(),
            _ => self.parse_simple_command(),
        }
    }

    fn parse_group(&mut self) -> Result<Command, ShellError> {
        self.advance()?; // eat '{'
        // Skip whitespace/newlines if needed? parse_list handles leading newlines?
        // No, Lexer handles whitespace. Newlines are tokens.
        // parse_list calls parse_pipeline calls parse_command.
        // If first token is Newline, parse_simple_command errors "Expected command...".
        // BUT '{' usually followed by space/cmd.
        // `{ \n cmd }` -> valid.
        // parse_list needs to handle optional leading newlines?
        // My parser doesn't handle leading newlines well yet.
        // Bash: `{` command `}`.
        let list = self.parse_list()?;
        match &self.current_token {
            Token::Word(w) if w == "}" => {
                self.advance()?;
                Ok(list)
            }
            _ => Err(ShellError::Syntax(
                "Expected '}' at end of block".to_string(),
            )),
        }
    }

    fn parse_for(&mut self) -> Result<Command, ShellError> {
        self.advance()?; // eat 'for'

        let variable = match &self.current_token {
            Token::Word(w) => w.clone(),
            _ => {
                return Err(ShellError::Syntax(
                    "Expected variable name after for".to_string(),
                ));
            }
        };
        self.advance()?;

        let mut items = Vec::new();

        // Check for 'in'
        match &self.current_token {
            Token::Word(w) if w == "in" => {
                self.advance()?; // eat 'in'

                loop {
                    match &self.current_token {
                        Token::Word(w) if w == "do" => break,
                        Token::Operator(op) if op == ";" => {
                            self.advance()?;
                            break;
                        }
                        Token::Newline => {
                            self.advance()?;
                            break;
                        }
                        Token::Word(w) => {
                            items.push(w.clone());
                            self.advance()?;
                        }
                        Token::Operator(_) => break, // e.g. | or & - unexpected usually
                        _ => break,
                    }
                }
            }
            // If no 'in', maybe just 'do' (iterate args - functionality for later)
            Token::Word(w) if w == "do" => {
                // No items provided? In bash this iterates over $@.
                // For now let's assume empty list or implement $@ logic later.
            }
            Token::Operator(op) if op == ";" => {
                self.advance()?;
            }
            Token::Newline => {
                self.advance()?;
            }
            _ => {
                return Err(ShellError::Syntax(
                    "Expected 'in' or 'do' in for loop".to_string(),
                ));
            }
        }

        // consume any extra separators before 'do'
        while match &self.current_token {
            Token::Operator(op) if op == ";" => true,
            Token::Newline => true,
            _ => false,
        } {
            self.advance()?;
        }

        match &self.current_token {
            Token::Word(w) if w == "do" => self.advance()?,
            _ => return Err(ShellError::Syntax("Expected 'do' in for loop".to_string())),
        };

        let body = self.parse_list()?;

        match &self.current_token {
            Token::Word(w) if w == "done" => self.advance()?,
            _ => {
                return Err(ShellError::Syntax(
                    "Expected 'done' at end of for loop".to_string(),
                ));
            }
        };

        Ok(Command::For {
            variable,
            items,
            body: Box::new(body),
        })
    }

    fn parse_if(&mut self) -> Result<Command, ShellError> {
        self.advance()?; // eat 'if'
        let condition = self.parse_list()?;

        match &self.current_token {
            Token::Word(w) if w == "then" => self.advance()?,
            _ => {
                return Err(ShellError::Syntax(
                    "Expected 'then' after if condition".to_string(),
                ));
            }
        };

        let then_body = self.parse_list()?;

        let else_body = match &self.current_token {
            Token::Word(w) if w == "else" => {
                self.advance()?;
                Some(Box::new(self.parse_list()?))
            }
            _ => None,
        };

        match &self.current_token {
            Token::Word(w) if w == "fi" => self.advance()?,
            _ => {
                return Err(ShellError::Syntax(
                    "Expected 'fi' at end of if statement".to_string(),
                ));
            }
        };

        Ok(Command::If {
            condition: Box::new(condition),
            then_body: Box::new(then_body),
            else_body,
        })
    }

    fn parse_while(&mut self) -> Result<Command, ShellError> {
        self.advance()?; // eat 'while'
        let condition = self.parse_list()?;

        match &self.current_token {
            Token::Word(w) if w == "do" => self.advance()?,
            _ => {
                return Err(ShellError::Syntax(
                    "Expected 'do' after while condition".to_string(),
                ));
            }
        };

        let body = self.parse_list()?;

        match &self.current_token {
            Token::Word(w) if w == "done" => self.advance()?,
            _ => {
                return Err(ShellError::Syntax(
                    "Expected 'done' at end of while loop".to_string(),
                ));
            }
        };

        Ok(Command::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_simple_command(&mut self) -> Result<Command, ShellError> {
        let mut args = Vec::new();
        let mut redirects = Vec::new();
        // Track indices of HereDoc redirects to process content later
        let mut heredoc_indices: Vec<usize> = Vec::new();

        loop {
            match self.current_token.clone() {
                Token::Word(w) => {
                    // Check for function def start: name ( )
                    if args.is_empty() && redirects.is_empty() {
                        let name = w.clone();
                        self.advance()?; // eat name

                        // Check if next is '('
                        if let Token::Operator(op) = &self.current_token {
                            if op == "(" {
                                self.advance()?; // eat '('

                                // Expect ')'
                                match &self.current_token {
                                    Token::Operator(op) if op == ")" => self.advance()?,
                                    _ => {
                                        return Err(ShellError::Syntax(
                                            "Expected ')' in function definition".to_string(),
                                        ));
                                    }
                                };

                                let body = self.parse_command()?;
                                return Ok(Command::FunctionDef {
                                    name,
                                    body: Box::new(body),
                                });
                            }
                        }

                        // Not a function def
                        args.push(name);
                        continue;
                    }

                    args.push(w);
                    self.advance()?;
                }

                Token::IoNumber(fd) => {
                    // Lexer guarantees that IoNumber is followed by < or >.
                    // Consumes IoNumber -> current is Operator
                    self.advance()?;

                    if let Token::Operator(op) = &self.current_token {
                        if matches!(
                            op.as_str(),
                            "<" | ">" | ">>" | "<<" | "<<<" | ">&" | "<&" | "&>"
                        ) {
                            let op_enum = match op.as_str() {
                                "<" => crate::ast::RedirectOp::Input,
                                ">" => crate::ast::RedirectOp::Output,
                                ">>" => crate::ast::RedirectOp::Append,
                                "<<" => crate::ast::RedirectOp::HereDoc,
                                "<<<" => crate::ast::RedirectOp::HereString,
                                ">&" => crate::ast::RedirectOp::DupOut,
                                "<&" => crate::ast::RedirectOp::DupIn,
                                "&>" => crate::ast::RedirectOp::AndOutput,
                                _ => unreachable!(),
                            };

                            self.advance()?; // Consumes Operator -> current is Target

                            if let Token::Word(target) = &self.current_token {
                                redirects.push(Redirect {
                                    fd,
                                    op: op_enum.clone(),
                                    target: target.clone(),
                                });

                                if op_enum == crate::ast::RedirectOp::HereDoc {
                                    heredoc_indices.push(redirects.len() - 1);
                                }

                                self.advance()?;
                            } else {
                                return Err(ShellError::Syntax(
                                    "Expected filename/delimiter after redirection".to_string(),
                                ));
                            }
                            continue;
                        }
                    }
                    // Fallback
                    args.push(fd.to_string());
                    continue;
                }
                Token::Operator(op)
                    if matches!(
                        op.as_str(),
                        "<" | ">" | ">>" | "<<" | "<<<" | ">&" | "<&" | "&>"
                    ) =>
                {
                    let op_enum = match op.as_str() {
                        "<" => crate::ast::RedirectOp::Input,
                        ">" => crate::ast::RedirectOp::Output,
                        ">>" => crate::ast::RedirectOp::Append,
                        "<<" => crate::ast::RedirectOp::HereDoc,
                        "<<<" => crate::ast::RedirectOp::HereString,
                        ">&" => crate::ast::RedirectOp::DupOut,
                        "<&" => crate::ast::RedirectOp::DupIn,
                        "&>" => crate::ast::RedirectOp::AndOutput,
                        _ => unreachable!(),
                    };
                    self.advance()?;

                    // For HereDoc/HereString, target is the delimiter/string.
                    // For Dup, target is FD number (as string).
                    // For others, filename.
                    if let Token::Word(target) = &self.current_token {
                        redirects.push(Redirect {
                            fd: match op_enum {
                                crate::ast::RedirectOp::Input
                                | crate::ast::RedirectOp::HereDoc
                                | crate::ast::RedirectOp::HereString
                                | crate::ast::RedirectOp::DupIn => 0,
                                _ => 1,
                            },
                            op: op_enum.clone(),
                            target: target.clone(),
                        });

                        if op_enum == crate::ast::RedirectOp::HereDoc {
                            heredoc_indices.push(redirects.len() - 1);
                        }

                        self.advance()?;
                    } else {
                        return Err(ShellError::Syntax(
                            "Expected filename/delimiter after redirection".to_string(),
                        ));
                    }
                }
                _ => break,
            }
        }

        // Process HereDocs if any
        if !heredoc_indices.is_empty() {
            for idx in heredoc_indices {
                let delimiter = redirects[idx].target.clone();
                let content = self.lexer.read_heredoc(&delimiter);
                redirects[idx].target = content;
            }

            if matches!(self.current_token, Token::Newline) {
                self.advance()?;
            }
        }

        if args.is_empty() && redirects.is_empty() {
            // It's possible we came here but found a terminator immediately?
            // "if true; then fi" -> Empty body?
            return Err(ShellError::Syntax(format!(
                "Expected command, found {:?}",
                self.current_token
            )));
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

    #[test]
    fn test_parser_if() {
        let input = "if true; then echo yes; else echo no; fi";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::If {
                condition: _,
                then_body: _,
                else_body,
            }) => {
                assert!(else_body.is_some());
            }
            _ => panic!("Expected if command, got {:?}", cmd),
        }
    }

    #[test]
    fn test_parser_while() {
        let input = "while true; do echo loop; done";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::While { .. }) => {}
            _ => panic!("Expected while command"),
        }
    }
    #[test]
    fn test_parser_for() {
        let input = "for i in a b c; do echo $i; done";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::For {
                variable, items, ..
            }) => {
                assert_eq!(variable, "i");
                assert_eq!(items, vec!["a", "b", "c"]);
            }
            _ => panic!("Expected for command"),
        }
    }

    #[test]
    fn test_parser_function() {
        let input = "foo() { echo bar; }";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::FunctionDef { name, body: _ }) => {
                assert_eq!(name, "foo");
            }
            _ => panic!("Expected function def"),
        }
    }

    #[test]
    fn test_parser_heredoc() {
        // "cat << EOF" -> RedirectOp::HereDoc with target "EOF"
        // (Execution handles reading the content)
        let input = "cat << EOF";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::Simple { redirects, .. }) => {
                assert_eq!(redirects.len(), 1);
                assert_eq!(redirects[0].op, crate::ast::RedirectOp::HereDoc);
                assert_eq!(redirects[0].target, "EOF");
            }
            _ => panic!("Expected simple command with heredoc"),
        }
    }

    #[test]
    fn test_parser_dup() {
        let input = "ls 2>&1";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer).unwrap();
        let cmd = parser.parse().unwrap();

        match cmd {
            Some(Command::Simple { redirects, .. }) => {
                assert_eq!(redirects.len(), 1);
                assert_eq!(redirects[0].op, crate::ast::RedirectOp::DupOut);
                assert_eq!(redirects[0].target, "1");
                assert_eq!(redirects[0].fd, 2); // 2>&...
            }
            _ => panic!("Expected simple command with dup"),
        }
    }
}
