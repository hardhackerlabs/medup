use std::fmt::{self, Debug};
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    // Create Ast object.
    let mut ast = Ast::new();

    // Get parameter from command line
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    // Open and read the markdown file
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);

    let mut counter = 0;
    for text in reader.lines() {
        counter += 1;
        let line = Line::parse(counter, text.unwrap());
        ast.push(line);
    }

    println!("{:?}", ast);
}

#[derive(PartialEq, Debug)]
enum TokenKind {
    Unknow,
    Mark,
    Content,
    NewLine,
}

struct Token {
    value: String,
    kind: TokenKind,
    line_num: i32,
}

#[derive(PartialEq)]
enum State {
    Begin,
    CheckMark,
    FinishMark,
    ToText,
}

struct Line {
    tokens: Vec<Token>,
}

impl Line {
    // parses one line text into Line that contains multi tokens.
    fn parse(ln: i32, text: String) -> Line {
        let mut tokens: Vec<Token> = Vec::new();

        let mut state: State = State::Begin;
        let mut start: usize = 0;

        for (current, ch) in text.chars().enumerate() {
            match state {
                // skip all whitespace characters at the beginning of the line.
                State::Begin => {
                    if ch.is_whitespace() {
                        continue;
                    }
                    start = current;
                    state = State::CheckMark;
                }
                // parse the first word in the line as the mark token.
                State::CheckMark => {
                    if !ch.is_whitespace() {
                        continue;
                    }
                    let first = &text[start..current];
                    let (kind, value) = match first {
                        "#" | "##" | "###" | "####" | "#####" => {
                            (TokenKind::Mark, first.to_string())
                        }
                        _ => (TokenKind::Unknow, first.to_string()),
                    };

                    if kind == TokenKind::Unknow {
                        state = State::ToText;
                    } else if kind == TokenKind::Mark {
                        state = State::FinishMark;
                    }
                    start = current;

                    tokens.push(Token {
                        value,
                        kind,
                        line_num: ln,
                    });
                }
                // skip all whitespace characters after the mark token.
                State::FinishMark => {
                    if ch.is_whitespace() {
                        continue;
                    }
                    start = current;
                    state = State::ToText;
                }
                // save the remaining characters into a token as text.
                State::ToText => {
                    let remain = &text[start..];
                    tokens.push(Token {
                        value: remain.to_string(),
                        kind: TokenKind::Content,
                        line_num: ln,
                    });
                    break;
                }
            };
        }

        if state == State::Begin {
            tokens.push(Token {
                value: text,
                kind: TokenKind::NewLine,
                line_num: ln,
            });
        }

        Line { tokens }
    }
}

struct Ast {
    lines: Vec<Line>,
}

impl Ast {
    fn new() -> Ast {
        Ast { lines: Vec::new() }
    }

    fn push(&mut self, line: Line) {
        self.lines.push(line);
    }
}

impl Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = String::new();
        for line in &self.lines {
            for t in &line.tokens {
                let s = format!("<{}, {}, {:?}> ", t.line_num, t.value, t.kind);
                debug.push_str(&s);
            }
            debug.push('\n');
        }
        writeln!(f, "{}", debug)
    }
}
