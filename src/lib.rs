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
                let s = format!("<{}, {}, {:?}> ", t.value, t.line_num, t.kind);
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
    TitleMark,
    DisorderMark,
    DividingMark,
    QuoteMark,
    Title,
    DisorderListItem,
    Quote,
    BlankLine,
    Plain,
}

// State is the state of the parser, it represents the current state of the parser.
#[derive(PartialEq)]
enum State {
    Begin,
    CheckMark,
    Title,
    DisorderedList,
    Quote,
    CheckDividing,
    Plain,
    Finished,
}

impl Line {
    // parses one line text into Line that contains multi tokens.
    pub fn parse(ln: i32, text: String) -> Line {
        let mut tokens: Vec<Token> = Vec::new();

        let mut state: State = State::Begin;
        let mut pointer: usize = 0;

        'next_char: for (current, ch) in text.chars().enumerate() {
            let mut state_times = 1;
            while state_times > 0 {
                state_times -= 1;

                match state {
                    // skip all whitespace characters at the beginning of the line.
                    State::Begin => {
                        if ch.is_whitespace() {
                            // parse blank line, blank line contains whitespace characters only.
                            if ch == '\n' {
                                tokens.push(Token {
                                    value: text.trim_end_matches('\n').to_string(),
                                    kind: TokenKind::BlankLine,
                                    line_num: ln,
                                });
                                state = State::Finished;
                            }
                        } else {
                            pointer = current;
                            state = State::CheckMark;
                        }
                    }

                    // parse the first word in the line as the mark token.
                    State::CheckMark => {
                        if !ch.is_whitespace() {
                            continue 'next_char;
                        }

                        let first_word = &text[pointer..current];

                        state = match first_word {
                            // title
                            "#" | "##" | "###" | "####" | "#####" => {
                                tokens.push(Token {
                                    value: first_word.to_string(),
                                    kind: TokenKind::TitleMark,
                                    line_num: ln,
                                });
                                pointer = current;
                                State::Title
                            }

                            // disordered list
                            "*" | "-" | "+" => {
                                tokens.push(Token {
                                    value: first_word.to_string(),
                                    kind: TokenKind::DisorderMark,
                                    line_num: ln,
                                });
                                pointer = current;
                                State::DisorderedList
                            }

                            // dividing line
                            // TODO: support more dividing line marks.
                            "***" | "---" | "___" => {
                                tokens.push(Token {
                                    value: first_word.to_string(),
                                    kind: TokenKind::DividingMark,
                                    line_num: ln,
                                });
                                pointer = current;
                                State::CheckDividing
                            }

                            // quote
                            ">" => {
                                tokens.push(Token {
                                    value: first_word.to_string(),
                                    kind: TokenKind::QuoteMark,
                                    line_num: ln,
                                });
                                pointer = current;
                                State::Quote
                            }

                            // plain (as no mark)
                            _ => {
                                // don't change the pointer, because the first word is not a mark.
                                // pointer = pointer;
                                State::Plain
                            }
                        }; // end of match first_word

                        if state == State::Plain {
                            state_times += 1;
                        }
                    } // end of CheckMark

                    // parse the rest of the line as the title token.
                    State::Title => {
                        // skip all whitespace characters after the mark token.
                        if ch.is_whitespace() {
                            continue 'next_char;
                        }
                        let rest = &text[current..];
                        tokens.push(Token {
                            value: rest.trim_end_matches('\n').to_string(),
                            kind: TokenKind::Title,
                            line_num: ln,
                        });
                        state = State::Finished;
                    }

                    // parse the rest of the line as the disordered list token.
                    State::DisorderedList => {
                        // skip all whitespace characters after the mark token.
                        if ch.is_whitespace() {
                            continue 'next_char;
                        }
                        let rest = &text[current..];
                        tokens.push(Token {
                            value: rest.trim_end_matches('\n').to_string(),
                            kind: TokenKind::DisorderListItem,
                            line_num: ln,
                        });
                        state = State::Finished;
                    }

                    // check if the line is a valid dividing line.
                    // TODO:
                    State::CheckDividing => {}

                    // parse the rest of the line as the quote token.
                    State::Quote => {
                        let rest = &text[current..];
                        tokens.push(Token {
                            value: rest.trim_end_matches('\n').to_string(),
                            kind: TokenKind::Quote,
                            line_num: ln,
                        });
                        state = State::Finished;
                    }

                    // parse the line as the plain token.
                    State::Plain => {
                        let content = &text[pointer..];
                        tokens.push(Token {
                            value: content.trim_end_matches('\n').to_string(),
                            kind: TokenKind::Plain,
                            line_num: ln,
                        });
                        state = State::Finished;
                    }

                    // finished
                    State::Finished => {
                        break 'next_char;
                    }
                } // end of match state
            } // end of loop
        } // end of for (next_char)

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
                } else if t.kind == TokenKind::TitleMark {
                    self.kind = LineKind::Title;
                } else if t.kind == TokenKind::Plain {
                    self.kind = LineKind::Content;
                }
            }
        }
    }
}
