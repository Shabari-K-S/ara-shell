// use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    // A simple command: "ls -la"
    Simple {
        args: Vec<String>,
        redirects: Vec<Redirect>,
        assignments: Vec<Assignment>,
    },
    // A pipeline: "cat file | grep foo"
    Pipeline(Vec<Command>),
    // A list: "cd /tmp && ls" (AND, OR, SEP)
    List {
        left: Box<Command>,
        op: ControlOp, // &&, ||, ;, &
        right: Box<Command>,
    },
    // A subshell: "( cd /tmp; ls )"
    Subshell(Box<Command>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Redirect {
    pub fd: i32,        // Default 0 for input, 1 for output
    pub op: RedirectOp, // >, <, >>, >&, etc.
    pub target: String, // Filename or FD number
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirectOp {
    Output,      // >
    Input,       // <
    Append,      // >>
    DupOutput,   // >&
    DupInput,    // <&
    ForceOutput, // >| (clobber)
    HereDoc,     // <<
    HereString,  // <<<
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlOp {
    And,   // &&
    Or,    // ||
    Semi,  // ;
    Async, // &
}
