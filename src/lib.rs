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

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = String::new();
        for line in &self.lines {
            debug.push_str(format!("{}: {:?}: ", line.num, line.kind).as_str());
            for t in &line.tokens {
                let s = format!("<{}, {:?}> ", t.value, t.kind);
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
    num: i32,
}

#[derive(Debug)]
enum LineKind {
    Unknow,
    Blank,
    Title,
    DisorderedList,
    DividingLine,
    Quote,
    Plain,
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

impl Line {
    // parses one line text into Line that contains multi tokens.
    pub fn parse(ln: i32, line: String) -> Line {
        let mut statem = Statem::new(&line);

        for (current, ch) in line.chars().enumerate() {
            let finished = statem.process(current, ch);
            if finished {
                break;
            }
        }
        let mut tokens = statem.finish();
        for mut t in &mut tokens {
            t.line_num = ln;
        }
        let kind = match tokens.first() {
            None => LineKind::Unknow,
            Some(t) => match t.kind {
                TokenKind::BlankLine => LineKind::Blank,
                TokenKind::TitleMark => LineKind::Title,
                TokenKind::DisorderMark => LineKind::DisorderedList,
                TokenKind::DividingMark => LineKind::DividingLine,
                TokenKind::QuoteMark => LineKind::Quote,
                TokenKind::Plain => LineKind::Plain,
                _ => LineKind::Unknow,
            },
        };

        Line {
            tokens,
            kind,
            num: ln,
        }
    }
}

// Statem represents the current state of the parser.
struct Statem<'a> {
    state: State,
    unparsed: usize,
    tokens: Vec<Token>,
    text: &'a String,
}

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

impl<'a> Statem<'a> {
    fn new(text: &'a String) -> Self {
        Statem {
            state: State::Begin,
            unparsed: 0,
            text,
            tokens: Vec::new(),
        }
    }

    fn process(&mut self, cur_pos: usize, cur_char: char) -> bool {
        let t = match self.state {
            State::Begin => self.skip_begin_whitespaces(cur_pos, cur_char),
            State::CheckMark => self.check_mark(cur_pos),
            State::Title => self.parse_title(cur_pos, cur_char),
            State::DisorderedList => self.parse_disordered_list(cur_pos, cur_char),
            State::CheckDividing => None,
            State::Quote => self.parse_quote(cur_pos, cur_char),
            State::Plain => self.parse_plain(cur_pos, cur_char),
            State::Finished => None,
        };

        if let Some(token) = t {
            self.tokens.push(token);
        }

        self.state == State::Finished
    }

    fn finish(mut self) -> Vec<Token> {
        let count = self.text.chars().count();
        if self.unparsed < count {
            self.process(count - 1, '\n');
        }
        self.tokens
    }

    // skip all whitespace characters at the beginning of the line,
    // or generate a blank line token if the line contains only whitespace characters.
    fn skip_begin_whitespaces(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            if cur_char == '\n' {
                self.state = State::Finished;
                self.unparsed = cur_pos + 1;
                return Some(Token {
                    value: "".to_string(),
                    kind: TokenKind::BlankLine,
                    line_num: 0,
                });
            }
        } else {
            self.state = State::CheckMark;
            self.unparsed = cur_pos;
        }
        None
    }

    fn find_word(&self, cur_pos: usize) -> Option<&str> {
        let ch = self.text.chars().nth(cur_pos).unwrap(); // note: unwrap, it is safe here.
        if ch.is_whitespace() {
            Some(&self.text[self.unparsed..cur_pos])
        } else {
            None
        }
    }

    // parse the first word in the line as the mark token
    fn check_mark(&mut self, cur_pos: usize) -> Option<Token> {
        let first_word = self.find_word(cur_pos)?;

        let (unparsed, state, token) = match first_word {
            // title
            "#" | "##" | "###" | "####" | "#####" => (
                cur_pos + 1,
                State::Title,
                Some(Token {
                    value: first_word.to_string(),
                    kind: TokenKind::TitleMark,
                    line_num: 0,
                }),
            ),

            // disordered list
            "*" | "-" | "+" => (
                cur_pos + 1,
                State::DisorderedList,
                Some(Token {
                    value: first_word.to_string(),
                    kind: TokenKind::DisorderMark,
                    line_num: 0,
                }),
            ),

            // dividing line
            // TODO: support more dividing line marksu
            "***" | "---" | "___" => (
                cur_pos + 1,
                State::CheckDividing,
                Some(Token {
                    value: first_word.to_string(),
                    kind: TokenKind::DividingMark,
                    line_num: 0,
                }),
            ),

            // quote
            ">" => (
                cur_pos + 1,
                State::Quote,
                Some(Token {
                    value: first_word.to_string(),
                    kind: TokenKind::QuoteMark,
                    line_num: 0,
                }),
            ),

            // plain (as no mark)
            _ => {
                // don't change the unparsed pointer, because the first word is not a mark.
                (self.unparsed, State::Plain, None)
            }
        };

        self.state = state;
        self.unparsed = unparsed;
        token
    }

    // parse the rest of the line as title token
    fn parse_title(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = &self.text[cur_pos..];
        self.unparsed = self.text.chars().count();
        self.state = State::Finished;
        Some(Token {
            value: rest.trim_end_matches('\n').to_string(),
            kind: TokenKind::Title,
            line_num: 0,
        })
    }

    // parse the rest of the line as the disordered list token.
    fn parse_disordered_list(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = &self.text[cur_pos..];
        self.unparsed = self.text.chars().count();
        self.state = State::Finished;
        Some(Token {
            value: rest.trim_end_matches('\n').to_string(),
            kind: TokenKind::DisorderListItem,
            line_num: 0,
        })
    }

    // parse the rest of the line as the quote token.
    fn parse_quote(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = &self.text[cur_pos..];
        self.unparsed = self.text.chars().count();
        self.state = State::Finished;
        Some(Token {
            value: rest.trim_end_matches('\n').to_string(),
            kind: TokenKind::Quote,
            line_num: 0,
        })
    }

    // parse the line as the plain token.
    fn parse_plain(&mut self, _cur_pos: usize, _cur_char: char) -> Option<Token> {
        let content = &self.text[self.unparsed..];
        self.unparsed = self.text.chars().count();
        self.state = State::Finished;
        Some(Token {
            value: content.trim_end_matches('\n').to_string(),
            kind: TokenKind::Plain,
            line_num: 0,
        })
    }
}
