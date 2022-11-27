use std::fmt;
use std::fmt::Debug;

// Token is a part of the line, the parser will parse the line into some tokens.
struct Token {
    value: String,
    kind: TokenKind,
    line_num: i32,
}

#[derive(PartialEq, Debug)]
enum TokenKind {
    Unknow,
    Mark,
    Content,
    BlankLine,
}

// Line is a line of the markdown file, it be parsed into some tokens.
pub struct Line {
    tokens: Vec<Token>,
    kind: LineKind,
}

enum LineKind {
    Unknow,
    Blank,
    Title,
    Content,
}

// State is the state of the parser, it represents the current state of the parser.
#[derive(PartialEq)]
enum State {
    Begin,
    CheckMark,
    FinishMark,
    ToContent,
}

impl Line {
    // parses one line text into Line that contains multi tokens.
    pub fn parse(ln: i32, text: String) -> Line {
        let mut tokens: Vec<Token> = Vec::new();
        let mut line_kind: LineKind = LineKind::Unknow;

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
                            state = State::FinishMark;
                            line_kind = LineKind::Title;
                            (TokenKind::Mark, first.to_string())
                        }
                        _ => {
                            state = State::ToContent;
                            line_kind = LineKind::Content;
                            (TokenKind::Unknow, first.to_string())
                        }
                    };
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
                    state = State::ToContent;
                }
                // save the remaining characters into a token as content.
                State::ToContent => {
                    let remain = &text[start..];
                    // TODO: parse content, because maybe there are some mark symbol in content.

                    tokens.push(Token {
                        value: remain.to_string(),
                        kind: TokenKind::Content,
                        line_num: ln,
                    });
                    break;
                }
            };
        }

        // parse blank line, blank line contains whitespace characters only.
        if state == State::Begin {
            tokens.push(Token {
                value: text,
                kind: TokenKind::BlankLine,
                line_num: ln,
            });
            line_kind = LineKind::Blank;
        }

        Line {
            tokens,
            kind: line_kind,
        }
    }
}

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
pub struct Ast {
    lines: Vec<Line>,
}

impl Ast {
    pub fn new() -> Ast {
        Ast { lines: Vec::new() }
    }

    pub fn push(&mut self, line: Line) {
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
