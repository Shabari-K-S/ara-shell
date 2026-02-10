// use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq)]
pub enum Command {
    // A simple command: "ls -la"
    Simple {
        args: Vec<Arg>,
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
    // If command: "if ...; then ...; else ...; fi"
    If {
        condition: Box<Command>,
        then_body: Box<Command>,
        else_body: Option<Box<Command>>,
    },
    // While command: "while ...; do ...; done"
    While {
        condition: Box<Command>,
        body: Box<Command>,
    },
    // For command: "for var in a b c; do ...; done"
    For {
        variable: String,
        items: Vec<String>,
        body: Box<Command>,
    },
    // Function definition: "foo() { ... }"
    FunctionDef {
        name: String,
        body: Box<Command>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Redirect {
    pub fd: i32,        // Default 0 for input, 1 for output
    pub op: RedirectOp, // >, <, >>, >&, etc.
    pub target: String, // Filename or FD number
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirectOp {
    Input,      // <
    Output,     // >
    Append,     // >>
    HereDoc,    // <<
    HereString, // <<<
    DupOut,     // >&
    DupIn,      // <&
    AndOutput,  // &> (stdout and stderr)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlOp {
    And,  // &&
    Or,   // ||
    Semi, // ;

    Async, // &
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Literal(String),
    ProcessSub {
        cmd: Box<Command>,
        direction: ProcessSubKind, // Input <() or Output >()
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProcessSubKind {
    Read,  // <(cmd)
    Write, // >(cmd)
}

// --- Display implementations for human-readable job output ---

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arg::Literal(s) => write!(f, "{}", s),
            Arg::ProcessSub { cmd, direction } => {
                let prefix = match direction {
                    ProcessSubKind::Read => "<",
                    ProcessSubKind::Write => ">",
                };
                write!(f, "{}({})", prefix, cmd)
            }
        }
    }
}

impl std::fmt::Display for Command {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Simple { args, .. } => {
                let parts: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "{}", parts.join(" "))
            }
            Command::Pipeline(cmds) => {
                let parts: Vec<String> = cmds.iter().map(|c| c.to_string()).collect();
                write!(f, "{}", parts.join(" | "))
            }
            Command::List { left, op, right } => {
                let op_str = match op {
                    ControlOp::And => "&&",
                    ControlOp::Or => "||",
                    ControlOp::Semi => ";",
                    ControlOp::Async => "&",
                };
                write!(f, "{} {} {}", left, op_str, right)
            }
            Command::If {
                condition,
                then_body,
                else_body,
            } => {
                write!(f, "if {}; then {}", condition, then_body)?;
                if let Some(eb) = else_body {
                    write!(f, "; else {}", eb)?;
                }
                write!(f, "; fi")
            }
            Command::While { condition, body } => {
                write!(f, "while {}; do {}; done", condition, body)
            }
            Command::For {
                variable,
                items,
                body,
            } => {
                write!(
                    f,
                    "for {} in {}; do {}; done",
                    variable,
                    items.join(" "),
                    body
                )
            }
            Command::FunctionDef { name, body } => {
                write!(f, "{}() {{ {} }}", name, body)
            }
            Command::Subshell(cmd) => write!(f, "({})", cmd),
        }
    }
}
