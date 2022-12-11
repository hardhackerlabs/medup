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
    SortedList,
    DividingLine,
    Quote,
    Plain,
}

// Token is a part of the line, the parser will parse the line into some tokens.
#[derive(PartialEq, Debug)]
enum Token {
    TitleMark(String),
    DisorderListMark(String),
    SortedListMark(String),
    DividingMark(String),
    QuoteMark(String),
    Title(String),
    ListItem(String),
    Quote(String),
    BlankLine(String),
    Plain(String),
    LineBreak(String),
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
    // parses a line of text into 'Line' struct that contains multi tokens.
    fn parse(&mut self) {
        let mut lx = Lexer::new(&self.text, &mut self.tokens);

        for (cur_pos, cur_char) in self.text.chars().enumerate() {
            let stopped = lx.process(cur_pos, cur_char);
            if stopped {
                break;
            }
        }
        lx.finish();

        self.kind = match self.tokens.first() {
            None => LineKind::Unknow,
            Some(t) => match t {
                Token::BlankLine(_) => LineKind::Blank,
                Token::TitleMark(_) => LineKind::Title,
                Token::DisorderListMark(_) => LineKind::DisorderedList,
                Token::SortedListMark(_) => LineKind::SortedList,
                Token::DividingMark(_) => LineKind::DividingLine,
                Token::QuoteMark(_) => LineKind::Quote,
                Token::Plain(_) => LineKind::Plain,
                _ => LineKind::Unknow,
            },
        }
    }
}

// Lexer used to split the text of the line into some tokens,
// It is implemented with a state machine.
struct Lexer<'a> {
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
    SortedList,
    Quote,
    CheckDividing,
    Plain(PlainState),
    Finished,
}

#[derive(PartialEq, Clone, Copy)]
enum PlainState {
    Init,
    LineBreak,
}

impl<'a> Lexer<'a> {
    fn new(text: &'a str, tokens: &'a mut Vec<Token>) -> Self {
        Lexer {
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
            State::DisorderedList => self.parse_list_item(cur_pos, cur_char),
            State::SortedList => self.parse_list_item(cur_pos, cur_char),
            State::CheckDividing => self.check_dividing(cur_pos, cur_char),
            State::Quote => self.parse_quote(cur_pos, cur_char),
            State::Plain(s) => self.parse_plain(s, cur_pos, cur_char),
            State::Finished => None,
        };

        if let Some(token) = t {
            self.line_tokens.push(token);
        }

        self.state == State::Finished
    }

    fn finish(&mut self) {
        let count = utf8_slice::len(self.line_text);
        if self.unparsed < count {
            self.process(count - 1, '\n');
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
        let first_word_chars: Vec<char> = first_word.chars().collect();

        let (unparsed, state, token) = match first_word_chars[..] {
            // title
            ['#'] | ['#', '#'] | ['#', '#', '#'] | ['#', '#', '#', '#'] => (
                cur_pos + 1,
                State::Title,
                Some(Token::TitleMark(first_word.to_string())),
            ),

            // disordered list
            ['*'] | ['-'] | ['+'] => (
                cur_pos + 1,
                State::DisorderedList,
                Some(Token::DisorderListMark(first_word.to_string())),
            ),

            // sorted list
            [n1, '.'] if n1 >= '1' && n1 <= '9' => (
                cur_pos + 1,
                State::SortedList,
                Some(Token::SortedListMark(first_word.to_string())),
            ),
            [n1, n2, '.'] if (n1 >= '1' && n1 <= '9') && (n2 >= '0' && n2 <= '9') => (
                cur_pos + 1,
                State::SortedList,
                Some(Token::SortedListMark(first_word.to_string())),
            ),
            [n1, n2, n3, '.']
                if (n1 >= '1' && n1 <= '9')
                    && (n2 >= '0' && n2 <= '9')
                    && (n3 >= '0' && n3 <= '9') =>
            {
                (
                    cur_pos + 1,
                    State::SortedList,
                    Some(Token::SortedListMark(first_word.to_string())),
                )
            }

            // dividing line
            ['*', '*', '*', ..] | ['-', '-', '-', ..] | ['_', '_', '_', ..]
                if first_word
                    .chars()
                    .filter(|x| !x.is_whitespace() && *x != '*' && *x != '-' && *x != '_')
                    .count()
                    == 0 =>
            {
                (
                    cur_pos + 1,
                    State::CheckDividing,
                    Some(Token::DividingMark(first_word.to_string())),
                )
            }

            // quote
            ['>'] => (
                cur_pos + 1,
                State::Quote,
                Some(Token::QuoteMark(first_word.to_string())),
            ),

            // plain (as no mark)
            _ => {
                // don't change the unparsed pointer, because the first word is not a mark.
                (self.unparsed, State::Plain(PlainState::Init), None)
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
        self.state = State::Plain(PlainState::Init);
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
        Some(Token::Title(rest.trim_end().to_string()))
    }

    // parse the rest of the line as a list item token.
    fn parse_list_item(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::ListItem(rest.trim_end().to_string()))
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
        Some(Token::Quote(rest.trim_end().to_string()))
    }

    // parse the line as the plain token.
    fn parse_plain(&mut self, sub: PlainState, _cur_pos: usize, _cur_char: char) -> Option<Token> {
        match sub {
            PlainState::Init => {
                let content = utf8_slice::from(self.line_text, self.unparsed);
                self.unparsed = utf8_slice::len(self.line_text);

                self.state = if Lexer::has_line_break(content) {
                    State::Plain(PlainState::LineBreak)
                } else {
                    State::Finished
                };
                Some(Token::Plain(content.trim_end().to_string()))
            }
            PlainState::LineBreak => {
                self.state = State::Finished;
                Some(Token::LineBreak("<br>".to_string()))
            }
        }
    }

    // find 'line break', double spaces or <br> at the end of the line
    fn has_line_break(s: &str) -> bool {
        let mut ws_count = 0;
        for ch in s.chars().rev() {
            if ch == '\n' {
                continue;
            }
            if ch.is_whitespace() {
                ws_count += 1;
                continue;
            }
            break;
        }
        if ws_count >= 2 {
            true
        } else {
            s.trim_end().ends_with("<br>")
        }
    }
}
