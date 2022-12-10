use std::fmt;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
pub struct Ast {
    lines: Vec<Line>,
}

impl Ast {
    pub fn new() -> Ast {
        Ast { lines: Vec::new() }
    }

    pub fn parse_file(&mut self, reader: BufReader<File>) {
        let mut counter = 0;
        for text in reader.lines() {
            counter += 1;
            let mut s = text.unwrap();
            s.push('\n');
            self.push(counter, s);
        }
    }

    fn push(&mut self, num: i32, line: String) {
        self.lines.push(Line::new(num, line));
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
            debug.push_str(format!("[{}, {:?}]: ", line.num, line.kind).as_str());
            for t in &line.tokens {
                let s = format!("{:?} ", t);
                debug.push_str(&s);
            }
            debug.push('\n');
        }
        writeln!(f, "{}", debug)
    }
}

// Line is a line of the markdown file, it be parsed into some tokens.
struct Line {
    tokens: Vec<Token>,
    kind: LineKind,
    num: i32,
    text: String,
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
#[derive(PartialEq, Debug)]
enum Token {
    TitleMark(String),
    DisorderMark(String),
    DividingMark(String),
    QuoteMark(String),
    Title(String),
    DisorderListItem(String),
    Quote(String),
    BlankLine(String),
    Plain(String),
}

impl Line {
    fn new(ln: i32, line: String) -> Self {
        let mut l = Line {
            num: ln,
            text: line,
            kind: LineKind::Unknow,
            tokens: Vec::new(),
        };
        l.parse();
        l
    }
    // parses one line text into Line that contains multi tokens.
    fn parse(&mut self) {
        let mut statem = Statem::new(&self.text, &mut self.tokens);

        for (cur_pos, cur_char) in self.text.chars().enumerate() {
            let finished = statem.process(cur_pos, cur_char);
            if finished {
                break;
            }
        }
        self.kind = statem.finish_and_calckind();
    }
}

// Statem represents the current state of the parser.
struct Statem<'a> {
    state: State,
    unparsed: usize,
    line_tokens: &'a mut Vec<Token>,
    line_text: &'a str,
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
    fn new(text: &'a str, tokens: &'a mut Vec<Token>) -> Self {
        Statem {
            state: State::Begin,
            unparsed: 0,
            line_text: text,
            line_tokens: tokens,
        }
    }

    fn process(&mut self, cur_pos: usize, cur_char: char) -> bool {
        let t = match self.state {
            State::Begin => self.skip_begin_whitespaces(cur_pos, cur_char),
            State::CheckMark => self.check_mark(cur_pos),
            State::Title => self.parse_title(cur_pos, cur_char),
            State::DisorderedList => self.parse_disordered_list(cur_pos, cur_char),
            State::CheckDividing => self.check_dividing(cur_pos, cur_char),
            State::Quote => self.parse_quote(cur_pos, cur_char),
            State::Plain => self.parse_plain(cur_pos, cur_char),
            State::Finished => None,
        };

        if let Some(token) = t {
            self.line_tokens.push(token);
        }

        self.state == State::Finished
    }

    fn finish_and_calckind(&mut self) -> LineKind {
        let count = utf8_slice::len(self.line_text);
        if self.unparsed < count {
            self.process(count - 1, '\n');
        }
        match self.line_tokens.first() {
            None => LineKind::Unknow,
            Some(t) => match t {
                Token::BlankLine(_) => LineKind::Blank,
                Token::TitleMark(_) => LineKind::Title,
                Token::DisorderMark(_) => LineKind::DisorderedList,
                Token::DividingMark(_) => LineKind::DividingLine,
                Token::QuoteMark(_) => LineKind::Quote,
                Token::Plain(_) => LineKind::Plain,
                _ => LineKind::Unknow,
            },
        }
    }

    // skip all whitespace characters at the beginning of the line,
    // or generate a blank line token if the line contains only whitespace characters.
    fn skip_begin_whitespaces(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            if cur_char == '\n' {
                self.state = State::Finished;
                self.unparsed = cur_pos + 1;
                return Some(Token::BlankLine("".to_string()));
            }
        } else {
            self.state = State::CheckMark;
            self.unparsed = cur_pos;
        }
        None
    }

    fn find_word(&self, cur_pos: usize) -> Option<&str> {
        let ch = self.line_text.chars().nth(cur_pos).unwrap(); // note: unwrap, it is safe here.
        if ch.is_whitespace() {
            Some(utf8_slice::slice(self.line_text, self.unparsed, cur_pos))
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
                Some(Token::TitleMark(first_word.to_string())),
            ),

            // disordered list
            "*" | "-" | "+" => (
                cur_pos + 1,
                State::DisorderedList,
                Some(Token::DisorderMark(first_word.to_string())),
            ),

            // dividing line
            "***" | "---" | "___" => (
                cur_pos + 1,
                State::CheckDividing,
                Some(Token::DividingMark(first_word.to_string())),
            ),

            // quote
            ">" => (
                cur_pos + 1,
                State::Quote,
                Some(Token::QuoteMark(first_word.to_string())),
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

    // if there is not a valid dividing line, will recreate the token as plain.
    fn check_dividing(&mut self, _cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            return None;
        }
        // if contains whitespace character, it's a invalid dividing line
        self.line_tokens.clear(); // not a valid dividing line, so clear the dividing mark token
        self.unparsed = 0;
        self.state = State::Plain;
        None
    }

    // parse the rest of the line as title token
    fn parse_title(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::Title(rest.trim_end_matches('\n').to_string()))
    }

    // parse the rest of the line as the disordered list token.
    fn parse_disordered_list(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::DisorderListItem(
            rest.trim_end_matches('\n').to_string(),
        ))
    }

    // parse the rest of the line as the quote token.
    fn parse_quote(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::Quote(rest.trim_end_matches('\n').to_string()))
    }

    // parse the line as the plain token.
    fn parse_plain(&mut self, _cur_pos: usize, _cur_char: char) -> Option<Token> {
        let content = utf8_slice::from(self.line_text, self.unparsed);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::Plain(content.trim_end_matches('\n').to_string()))
    }
}
