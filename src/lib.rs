use std::fmt;
use std::fmt::Debug;

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

// Token is a part of the line, the parser will parse the line into some tokens.
struct Token {
    value: String,
    kind: TokenKind,
    line_num: i32,
}

#[derive(PartialEq, Debug)]
enum TokenKind {
    Mark,
    Title,
    Content,
    BlankLine,
}

// State is the state of the parser, it represents the current state of the parser.
#[derive(PartialEq)]
enum State {
    Begin,
    CheckMark,
    ToTitle,
    ToRestContent,
}

impl Line {
    // parses one line text into Line that contains multi tokens.
    pub fn parse(ln: i32, text: String) -> Line {
        let mut tokens: Vec<Token> = Vec::new();

        let mut state: State = State::Begin;
        let mut start: usize = 0;

        for (current, ch) in text.chars().enumerate() {
            match state {
                // skip all whitespace characters at the beginning of the line.
                State::Begin => {
                    if ch.is_whitespace() {
                        // parse blank line, blank line contains whitespace characters only.
                        if ch == '\n' {
                            tokens.push(Token {
                                value: text,
                                kind: TokenKind::BlankLine,
                                line_num: ln,
                            });
                            break;
                        }
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
                    start = current;

                    match first {
                        // title
                        "#" | "##" | "###" | "####" | "#####" => {
                            tokens.push(Token {
                                value: first.to_string(),
                                kind: TokenKind::Mark,
                                line_num: ln,
                            });
                            state = State::ToTitle;
                        }
                        // content
                        _ => {
                            tokens.push(Token {
                                value: first.to_string(),
                                kind: TokenKind::Content,
                                line_num: ln,
                            });
                            state = State::ToRestContent;
                        }
                    }
                }
                // parse the rest of the line as the title token.
                State::ToTitle => {
                    // skip all whitespace characters after the mark token.
                    if ch.is_whitespace() {
                        continue;
                    }
                    let rest = &text[current..];
                    tokens.push(Token {
                        value: rest.trim_end_matches('\n').to_string(),
                        kind: TokenKind::Title,
                        line_num: ln,
                    });
                    break;
                }
                // parse the rest of the line as the content token.
                State::ToRestContent => {
                    let rest = &text[start..];
                    tokens.push(Token {
                        value: rest.trim_end_matches('\n').to_string(),
                        kind: TokenKind::Content,
                        line_num: ln,
                    });
                    break;
                }
            };
        }

        let mut l = Line {
            tokens,
            kind: LineKind::Unknow,
        };
        l.parse_inside();
        l
    }

    fn parse_inside(&mut self) {
        let tokens = &self.tokens;

        match tokens.first() {
            None => {
                self.kind = LineKind::Unknow;
            }
            Some(t) => {
                if t.kind == TokenKind::BlankLine {
                    self.kind = LineKind::Blank;
                } else if t.kind == TokenKind::Mark {
                    self.kind = LineKind::Title;
                } else if t.kind == TokenKind::Content {
                    self.kind = LineKind::Content;
                }
            }
        }

        for t in tokens {
            match t.kind {
                TokenKind::Content => (),
                _ => (),
            }
        }
    }
}
