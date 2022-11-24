use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    // Get parameter from command line
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    // Create Ast object.
    let mut ast = Ast::new();

    // Open and read the markdown file
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);

    let mut counter = 0;
    for line in reader.lines() {
        counter += 1;
        let s = line.unwrap();
        let tokens = parse_line_tokens(counter, &s);

        ast.push(tokens);
    }

    println!("{:?}", ast);
}

#[derive(PartialEq, Copy, Clone, Debug)]
enum TokenKind {
    Unknow,
    Mark,
    Text,
    NewLine,
}

#[derive(Debug)]
struct Token {
    value: String,
    kind: TokenKind,
    line_num: i32,
}

#[derive(PartialEq)]
enum State {
    Begin,
    GuessMark,
    MarkDone,
    ToText,
}

// parses one line text into multi tokens.
fn parse_line_tokens(ln: i32, line: &String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut state = State::Begin;
    let mut token_start: usize = 0;

    for (i, ch) in line.chars().enumerate() {
        match state {
            // skip all whitespace characters at the beginning of the line.
            State::Begin => {
                if ch.is_whitespace() {
                    continue;
                }
                token_start = i;
                state = State::GuessMark;
            }
            // parse the first word in the line as the mark token.
            State::GuessMark => {
                if !ch.is_whitespace() {
                    continue;
                }
                let first = &line[token_start..i];
                let (kind, value) = match first {
                    "#" | "##" | "###" | "####" | "#####" => (TokenKind::Mark, first.to_string()),
                    _ => (TokenKind::Unknow, first.to_string()),
                };

                tokens.push(Token {
                    value: value,
                    kind: kind,
                    line_num: ln,
                });

                if kind == TokenKind::Mark {
                    state = State::MarkDone;
                } else {
                    state = State::ToText;
                }
                token_start = i;
            }
            // skip all whitespace characters after the mark token.
            State::MarkDone => {
                if ch.is_whitespace() {
                    continue;
                }
                token_start = i;
                state = State::ToText;
            }
            // save the remaining characters into a token as text.
            State::ToText => {
                let remain = &line[token_start..line.len()];
                tokens.push(Token {
                    value: remain.to_string(),
                    kind: TokenKind::Text,
                    line_num: ln,
                });
                break;
            }
        };
    }

    if state == State::Begin {
        tokens.push(Token {
            value: line.to_string(),
            kind: TokenKind::NewLine,
            line_num: ln,
        });
    }

    tokens
}

#[derive(Debug)]
struct Ast {
    lines: Vec<Vec<Token>>,
}

impl Ast {
    fn new() -> Ast {
        Ast { lines: Vec::new() }
    }

    fn push(&mut self, line: Vec<Token>) {
        self.lines.push(line);
    }
}
