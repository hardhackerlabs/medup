use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;
use std::{fmt, io};

use itertools::Itertools;

pub type SharedLine = Rc<RefCell<Line>>;

pub trait HtmlGenerate {
    fn gen_title(&self, l: &SharedLine) -> String;
    fn gen_dividling(&self, l: &SharedLine) -> String;
    fn gen_normal(&self, l: &SharedLine) -> String;
    fn gen_blank(&self, ls: &[SharedLine]) -> String;
    fn gen_sorted_list(&self, ls: &[SharedLine]) -> String;
    fn gen_disordered_list(&self, ls: &[SharedLine]) -> String;
    fn gen_quote(&self, ls: &[SharedLine]) -> String;
    fn gen_code(&self, ls: &[SharedLine]) -> String;
}

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
pub struct Ast {
    document: Vec<SharedLine>,
    blocks: Vec<Block>,
    defer_queue: Vec<SharedLine>,
    enabled_defer: bool,
}

impl Ast {
    // Create a Ast instance.
    pub fn new() -> Self {
        Ast {
            document: vec![Rc::new(RefCell::new(Line::meta()))],
            blocks: vec![],
            defer_queue: vec![],
            enabled_defer: false,
        }
    }

    // Parse markdown document from a file, the 'path' argument is the file path.
    pub fn parse_file(&mut self, path: &str) -> Result<(), io::Error> {
        let file = File::open(path)?;
        self.parse_reader(&mut BufReader::new(file))
    }

    // Parse markdown document from a string.
    pub fn parse_string(&mut self, s: &str) -> Result<(), io::Error> {
        self.parse_reader(&mut s.as_bytes())
    }

    // Parse markdown document from a reader, the 'reader' may be a file reader, byte buff or network socket etc.
    pub fn parse_reader(&mut self, reader: &mut dyn BufRead) -> Result<(), io::Error> {
        let mut num: usize = 0;
        loop {
            let mut buf = String::new();
            let num_bytes = reader.read_line(&mut buf)?;
            if num_bytes == 0 {
                break;
            }
            if !buf.ends_with('\n') {
                buf.push('\n');
            }
            num += 1;
            self.parse_line(num, buf);
        }
        self.defer_queue.iter().for_each(|l| l.borrow_mut().parse());
        self.defer_queue.clear();
        self.blocks = Ast::establish_blocks(&self.document);
        Ok(())
    }

    // Iterate through each block of the Ast and process the block into a 'html' string
    pub fn to_html(&self, html: &impl HtmlGenerate) -> Result<String, Box<dyn Error>> {
        let ss: Vec<String> = vec![
            String::from(
                "<!doctype html>
<html>
<head>
<meta charset='UTF-8'><meta name='viewport' content='width=device-width initial-scale=1'>
<title></title>
</head>
<body>",
            ),
            String::from("</body></html>"),
        ];

        let gens = self
            .blocks()
            .iter()
            .filter(|b| b.kind() != Kind::Meta && b.kind() != Kind::ListNesting)
            .map(|b| match b.kind() {
                Kind::Title => html.gen_title(b.first()),
                Kind::NormalText => html.gen_normal(b.first()),
                Kind::DividingLine => html.gen_dividling(b.first()),
                Kind::Code => html.gen_code(b.lines()),
                Kind::DisorderedList => html.gen_disordered_list(b.lines()),
                Kind::Blank => html.gen_blank(b.lines()),
                Kind::Quote => html.gen_quote(b.lines()),
                Kind::SortedList => html.gen_sorted_list(b.lines()),
                _ => "".to_string(),
            })
            .join("\n");

        Ok(Itertools::intersperse(ss.iter(), &gens).join("\n"))
    }

    // Count the lines in ast
    pub fn count_lines(&self) -> usize {
        self.document.len() - 1
    }

    // Get the Line object with line number
    pub fn get_line(&self, num: usize) -> Option<&SharedLine> {
        self.document.get(num)
    }

    fn blocks(&self) -> &Vec<Block> {
        &self.blocks
    }

    fn parse_line(&mut self, num: usize, line: String) {
        let l = if self.defer_parse(&line) {
            let c = RefCell::new(Line::new_without_parsing(num, line));
            let l1 = Rc::new(c);
            let l2 = Rc::clone(&l1);
            self.defer_queue.push(l1);
            l2
        } else {
            let c = RefCell::new(Line::new(num, line));
            Rc::new(c)
        };
        if l.borrow().kind == Kind::CodeMark {
            self.enabled_defer = !self.enabled_defer;
            if !self.enabled_defer {
                // closed code block
                self.defer_queue.clear();
            }
        }
        self.document.push(l);

        debug_assert_eq!(self.count_lines(), num);
        debug_assert_eq!(self.document[num].borrow().num, num);
    }

    fn defer_parse(&self, s: &str) -> bool {
        self.enabled_defer && s.trim() != "```"
    }

    fn establish_blocks(all: &[SharedLine]) -> Vec<Block> {
        let mut blocks: Vec<Block> = vec![];

        let mut leader: Option<&SharedLine> = None;
        let mut state: Option<Kind> = None;

        let mut iter = all.iter().peekable();
        while let Some(l) = iter.next() {
            let current = l.borrow();
            match state.unwrap_or(current.kind) {
                Kind::Meta => {}

                Kind::DisorderedList | Kind::SortedList => {
                    if let Some(b) = blocks.last_mut().filter(|b| b.kind() == current.kind) {
                        b.push(Rc::clone(l));
                    } else {
                        Ast::insert_block(&mut blocks, Block::new(Rc::clone(l), current.kind))
                    }

                    // Check the next line is a list nesting or not
                    if let Some(next) = iter.peek() {
                        let next = next.borrow();
                        if next.indents() - current.indents() == 1 // indent 1
                            && next.kind != Kind::Blank
                            && next.kind != Kind::Title
                            && next.kind != Kind::DividingLine
                            && next.kind != Kind::CodeMark
                            && next.kind != Kind::Code
                        {
                            state = Some(Kind::ListNesting);
                            leader = Some(l); // save the previous line object as leader
                        }
                    }
                }
                Kind::ListNesting => {
                    // leader must not be None
                    debug_assert!(leader.is_some());

                    if let Some(leader) = leader {
                        let mut leader = leader.borrow_mut();
                        leader.nesting_lines.push(Rc::clone(l));

                        if let Some(next) = iter.peek() {
                            let next = next.borrow();
                            if next.indents() - leader.indents() >= 1 // at least indent 1
                            && next.kind != Kind::Blank
                            && next.kind != Kind::Title
                            && next.kind != Kind::DividingLine
                            && next.kind != Kind::CodeMark
                            && next.kind != Kind::Code
                            {
                                // keep the state
                            } else {
                                state = None;
                            }
                        }
                    }
                }

                Kind::CodeMark => {
                    let mut k: Option<Kind> = None;
                    if let Some(next) = iter.peek() {
                        let next = next.borrow();
                        if next.kind == Kind::CodeMark || next.kind == Kind::Code {
                            k = Some(Kind::Code);
                            state = Some(Kind::Code);
                        }
                    }
                    Ast::insert_block(
                        &mut blocks,
                        Block::new(Rc::clone(l), k.unwrap_or(Kind::CodeMark)),
                    );
                }
                Kind::Code => {
                    let b = blocks.last_mut().filter(|b| b.kind() == Kind::Code);
                    debug_assert!(b.is_some());

                    if let Some(b) = b {
                        b.push(Rc::clone(l));
                    }

                    if current.kind == Kind::CodeMark {
                        // close this code block
                        state = None;
                    }
                }

                Kind::Blank | Kind::Quote => {
                    if let Some(b) = blocks.last_mut().filter(|b| b.kind() == current.kind) {
                        b.push(Rc::clone(l));
                    } else {
                        Ast::insert_block(&mut blocks, Block::new(Rc::clone(l), current.kind))
                    }
                }

                Kind::NormalText | Kind::DividingLine | Kind::Title => {
                    Ast::insert_block(&mut blocks, Block::new(Rc::clone(l), current.kind))
                }
            }
        }

        for b in blocks
            .iter()
            .filter(|b| b.kind() == Kind::DisorderedList || b.kind() == Kind::SortedList)
        {
            b.lines()
                .iter()
                .filter(|l| !l.borrow().nesting_lines.is_empty())
                .for_each(|l| {
                    let mut l = l.borrow_mut();
                    let mut bs = Ast::establish_blocks(&l.nesting_lines);
                    l.nesting_blocks.append(&mut bs);
                });
        }

        blocks
    }

    fn insert_block(blocks: &mut Vec<Block>, mut b: Block) {
        b.seq = blocks.len();
        blocks.push(b);
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
        for line in self.document.iter() {
            debug.push_str(format!("[{}, {:?}]: ", line.borrow().num, line.borrow().kind).as_str());
            for t in line.borrow().all() {
                let s = format!("{:?} ", t);
                debug.push_str(&s);
            }
            debug.push('\n');
        }
        writeln!(f, "{}", debug)
    }
}

// Block is a group of multiple lines.
#[derive(Debug)]
struct Block {
    indices: Vec<SharedLine>,
    kind: Kind,
    seq: usize,
}

impl Block {
    fn new(l: SharedLine, kind: Kind) -> Self {
        Block {
            indices: vec![l],
            kind,
            seq: 0,
        }
    }
    fn lines(&self) -> &Vec<SharedLine> {
        &self.indices
    }

    fn first(&self) -> &SharedLine {
        &self.indices[0]
    }

    fn kind(&self) -> Kind {
        self.kind
    }

    fn push(&mut self, l: SharedLine) {
        self.indices.push(l)
    }

    fn _count(&self) -> usize {
        self.indices.len()
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Kind {
    NormalText,
    Blank,
    Title,
    DisorderedList,
    SortedList,
    DividingLine,
    Quote,
    CodeMark,
    Code,
    Meta,
    ListNesting,
}

// Line is a line of the markdown file, it be parsed into some tokens.
#[derive(Debug)]
pub struct Line {
    buff: Vec<Token>,
    kind: Kind,
    num: usize,
    text: String,
    //
    // The lines in nesting:
    //   Kind::DisorderedList
    //   Kind::SortedList
    //   Kind::Quote
    //   Kind::Normal
    nesting_lines: Vec<SharedLine>,
    nesting_blocks: Vec<Block>,
}

impl Line {
    // Get the mark token in the Line, the mark token may be the first or second
    pub fn get_mark(&self) -> &Token {
        let first = self.first_token();
        if first.kind() == TokenKind::WhiteSpace {
            // if the first token is 'WhiteSpace', the second token must be exist
            &self.buff[1]
        } else {
            first
        }
    }

    // Get the Nth Token in the Line
    pub fn get(&self, at: usize) -> Option<&Token> {
        self.buff.get(at)
    }

    // Get all tokens in the Line
    pub fn all(&self) -> &Vec<Token> {
        &self.buff
    }

    // Get the line text
    pub fn text(&self) -> &str {
        &self.text
    }

    fn new(ln: usize, line: String) -> Self {
        let mut l = Line {
            num: ln,
            text: line,
            kind: Kind::NormalText,
            buff: vec![],
            nesting_lines: vec![],
            nesting_blocks: vec![],
        };
        l.parse();
        l
    }

    fn new_without_parsing(ln: usize, line: String) -> Self {
        Line {
            num: ln,
            text: line,
            kind: Kind::Code,
            buff: vec![],
            nesting_lines: vec![],
            nesting_blocks: vec![],
        }
    }

    fn meta() -> Self {
        Line {
            buff: vec![],
            kind: Kind::Meta,
            num: 0,
            text: "meta".to_string(),
            nesting_lines: vec![],
            nesting_blocks: vec![],
        }
    }

    // Parses a line of text into 'Line' struct that contains multi tokens.
    // Line's kind is determinded by the first token's kind.
    fn parse(&mut self) {
        self.buff = Lexer::new(&self.text).split();

        self.kind = match self.get_mark().kind() {
            TokenKind::BlankLine => Kind::Blank,
            TokenKind::TitleMark => Kind::Title,
            TokenKind::DisorderListMark => Kind::DisorderedList,
            TokenKind::SortedListMark => Kind::SortedList,
            TokenKind::DividingMark => Kind::DividingLine,
            TokenKind::QuoteMark => Kind::Quote,
            TokenKind::CodeMark => Kind::CodeMark,
            _ => Kind::NormalText,
        };

        debug_assert!(!self.all().is_empty());
    }

    fn indents(&self) -> usize {
        let first = self.first_token();
        if first.kind() != TokenKind::WhiteSpace {
            return 0;
        }
        let l = first.value().len();
        if first.value().chars().filter(|c| *c == '\t').count() == l {
            return l;
        }
        if first.value().chars().filter(|c| *c == ' ').count() == l && l % 2 == 0 {
            l / 2
        } else {
            0
        }
    }

    fn first_token(&self) -> &Token {
        &self.buff[0]
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    TitleMark,        // #, ##, ###, ####
    DisorderListMark, // *
    SortedListMark,   // 1.
    DividingMark,     // ---, ***, ___
    QuoteMark,        // >
    BoldMark,         // ** **
    ItalicMark,       // * *
    ItalicBoldMark,   // *** ***
    CodeMark,         // ```
    BlankLine,        // \n
    LineBreak,        // <br>, double whitespace
    Image,            // ![]()
    Url,              // []()
    Text,
    StarMark,
    WhiteSpace,
}

// Token is a part of the line, the parser will parse the line into some tokens.
#[derive(PartialEq, Debug)]
pub struct Token {
    value: String,
    kind: TokenKind,
    details: Option<HashMap<String, String>>,
}

impl Token {
    fn new(value: String, kind: TokenKind) -> Self {
        Token {
            value,
            kind,
            details: None,
        }
    }

    fn split_off(&mut self, at: usize) -> Token {
        let off = self.value.split_off(at);
        Token::new(off, self.kind())
    }

    fn insert(&mut self, k: &str, v: &str) {
        self.details
            .get_or_insert(HashMap::new())
            .insert(k.to_string(), v.to_string());
    }

    fn insert_show_name(&mut self, v: &str) {
        self.insert("show_name", v)
    }

    fn insert_location(&mut self, v: &str) {
        self.insert("location", v)
    }

    // Get value of the token
    pub fn value(&self) -> &str {
        &self.value
    }

    // Get the value length of the token
    pub fn len(&self) -> usize {
        self.value.len()
    }

    // Check the value of the token is empty or not
    pub fn is_empty(&self) -> bool {
        self.value.len() == 0
    }

    // Get kind of the token
    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    // convert the token to url token
    pub fn into_url(&self) -> UrlToken {
        UrlToken(self)
    }

    // convert the token to img token
    pub fn into_img(&self) -> ImgToken {
        ImgToken(self)
    }
}

#[derive(PartialEq, Debug)]
pub struct UrlToken<'url_token>(&'url_token Token);

impl<'url_token> UrlToken<'url_token> {
    // Get show name of the URL
    pub fn get_show_name(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("show_name").map(|x| &**x))
    }

    // Get location of the URL
    pub fn get_location(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("location").map(|x| &**x))
    }
}

#[derive(PartialEq, Debug)]
pub struct ImgToken<'img_token>(&'img_token Token);

impl<'img_token> ImgToken<'img_token> {
    // Get alt name of the Image
    pub fn get_alt_name(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("show_name").map(|x| &**x))
    }

    // Get location of the Image
    pub fn get_location(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("location").map(|x| &**x))
    }
}

#[derive(PartialEq, Debug)]
enum State {
    Begin,
    Mark(usize),
    VerifyDividing(usize),
    Normal(usize),
}

#[derive(PartialEq, Clone, Copy)]
enum TextState {
    Normal,
    // means *, usize is start position in line
    Star(usize),
    // means !, usize is the index of '!'
    PicBegin(usize),
    // means [, (usize, usize) is the index of ('!', '[')
    PicTitleBegin(usize, usize),
    // means [, usize is the index of '['
    UrlTitleBegin(usize),
    // means ], (usize, usize, usize) is the index of ('!', '[', ']')
    TitleDone(Option<usize>, usize, usize),
    // means (, (usize, usize, usize, usize) is the index of ('!', '[', ']', '(')
    LocationBegin(Option<usize>, usize, usize, usize),
}

// Lexer is a lexical analyzer that parses lines of text into multiple tokens.
struct Lexer<'lex> {
    state: State,
    line_text: &'lex str,
}

impl<'lex> Lexer<'lex> {
    fn new(text: &'lex str) -> Self {
        Lexer {
            state: State::Begin,
            line_text: text,
        }
    }

    fn split(mut self) -> Vec<Token> {
        let mut buff = vec![];
        let iter = self.line_text.chars().enumerate().peekable();
        for (ix, curr) in iter {
            match self.state {
                State::Begin => {
                    if !curr.is_whitespace() {
                        let s = self.slice(0, ix);
                        if !s.is_empty() {
                            buff.push(Token::new(s, TokenKind::WhiteSpace));
                        }
                        self.goto(State::Mark(ix));
                    } else {
                        // the end of iterator
                        if curr == '\n' {
                            buff.push(Token::new(self.slice(0, ix), TokenKind::BlankLine));
                        } else {
                            // keep this state
                        }
                    }
                }

                State::Mark(begin) => {
                    // find the first word
                    let first_word = if curr.is_whitespace() {
                        // the current character is white space
                        self.slice_str(begin, ix)
                    } else {
                        continue;
                    };

                    if let Some(m) = self.split_mark(first_word) {
                        match m.kind() {
                            TokenKind::DividingMark => self.goto(State::VerifyDividing(begin)),
                            TokenKind::CodeMark => self.goto(State::Normal(begin + 3)),
                            _ => self.goto(State::Normal(ix + 1)),
                        }
                        buff.push(m);
                    } else {
                        // normal text
                        self.goto(State::Normal(begin));
                    }
                }
                State::VerifyDividing(p) => {
                    if !curr.is_whitespace() {
                        // if contains other characters, it's a invalid dividing line
                        // so remove the dividing mark token
                        buff.pop();
                        self.goto(State::Normal(p));
                    }
                }
                State::Normal(_) => {
                    break;
                }
            };
        }

        if let State::Normal(begin) = self.state {
            let rest = self.slice_rest(begin);
            let mut ts = Self::split_inside(rest);
            if Lexer::has_br(rest) {
                ts.push(Token::new("<br>".to_string(), TokenKind::LineBreak));
            }
            for t in ts.into_iter().filter(|t| !t.value().is_empty()) {
                buff.push(t);
            }
        }
        buff
    }

    fn goto(&mut self, state: State) {
        self.state = state;
    }

    fn slice_rest(&self, begin: usize) -> &str {
        utf8_slice::from(self.line_text, begin)
    }

    fn slice_str(&self, begin: usize, end: usize) -> &str {
        utf8_slice::slice(self.line_text, begin, end)
    }

    fn slice(&self, begin: usize, end: usize) -> String {
        self.slice_str(begin, end).to_string()
    }

    // parse the first word in the line as the mark token
    fn split_mark(&self, first_word: &str) -> Option<Token> {
        let first_word_chars: Vec<char> = first_word.chars().collect();

        match first_word_chars[..] {
            // title
            ['#'] | ['#', '#'] | ['#', '#', '#'] | ['#', '#', '#', '#'] => {
                Some(Token::new(first_word.to_string(), TokenKind::TitleMark))
            }
            // disordered list
            ['*'] | ['-'] | ['+'] => Some(Token::new(
                first_word.to_string(),
                TokenKind::DisorderListMark,
            )),
            // sorted list
            [n1, '.'] if ('1'..='9').contains(&n1) => Some(Token::new(
                first_word.to_string(),
                TokenKind::SortedListMark,
            )),
            [n1, n2, '.'] if ('1'..='9').contains(&n1) && ('0'..='9').contains(&n2) => Some(
                Token::new(first_word.to_string(), TokenKind::SortedListMark),
            ),
            [n1, n2, n3, '.']
                if ('1'..='9').contains(&n1)
                    && ('0'..='9').contains(&n2)
                    && ('0'..='9').contains(&n3) =>
            {
                Some(Token::new(
                    first_word.to_string(),
                    TokenKind::SortedListMark,
                ))
            }
            // dividing line
            ['*', '*', '*', ..] | ['-', '-', '-', ..] | ['_', '_', '_', ..]
                if first_word
                    .chars()
                    .filter(|x| !x.is_whitespace() && *x != '*' && *x != '-' && *x != '_')
                    .count()
                    == 0 =>
            {
                Some(Token::new(first_word.to_string(), TokenKind::DividingMark))
            }
            // quote
            ['>'] => Some(Token::new(first_word.to_string(), TokenKind::QuoteMark)),
            // code block mark
            // .e.g:
            //      ```rust
            //      ``` rust
            ['`', '`', '`', ..] => Some(Token::new("```".to_string(), TokenKind::CodeMark)),
            // normal (as no mark)
            _ => {
                // don't change the unparsed pointer, because the first word is not a mark.
                None
            }
        }
    }

    // parse text content, include bold, picture and url etc.
    fn split_inside(content: &str) -> Vec<Token> {
        let mut last = 0;

        let mut buff: Vec<Token> = Vec::new();
        let mut state = TextState::Normal;

        let mut content_iter = content.chars().enumerate().peekable();
        while let Some((i, ch)) = content_iter.next() {
            match (state, ch) {
                (_, '\n') => {
                    // end of the line
                    let s = utf8_slice::slice(content, last, i)
                        .trim_end()
                        .trim_end_matches("<br>")
                        .to_string();
                    if !s.is_empty() {
                        buff.push(Token::new(s, TokenKind::Text));
                    }
                    break;
                }
                (TextState::Normal, _) => match ch {
                    '*' => {
                        // the part of normal text before '*' mark.
                        let s = utf8_slice::slice(content, last, i);
                        if !s.is_empty() {
                            buff.push(Token::new(s.to_string(), TokenKind::Text));
                        }

                        last = i;

                        if let Some((_, n)) = content_iter.peek() {
                            if *n == '*' {
                                state = TextState::Star(i);
                            } else {
                                // '*' mark
                                let s = utf8_slice::slice(content, i, i + 1);
                                buff.push(Token::new(s.to_string(), TokenKind::StarMark));

                                last = i + 1;
                            }
                        }
                    }
                    '!' => state = TextState::PicBegin(i), // begin of picture
                    '[' => state = TextState::UrlTitleBegin(i), // begin of url
                    _ => (),
                },
                (TextState::PicBegin(p), _) => match ch {
                    '[' => state = TextState::PicTitleBegin(p, i),
                    '!' => state = TextState::PicBegin(i),
                    _ => state = TextState::Normal,
                },
                (TextState::PicTitleBegin(p1, p2), _) => {
                    if ch == ']' {
                        state = TextState::TitleDone(Some(p1), p2, i);
                    }
                }
                (TextState::UrlTitleBegin(p), _) => match ch {
                    ']' => state = TextState::TitleDone(None, p, i),
                    '[' => state = TextState::UrlTitleBegin(i),
                    _ => (),
                },
                (TextState::TitleDone(p1, p2, p3), _) => match ch {
                    '(' => state = TextState::LocationBegin(p1, p2, p3, i),
                    ']' => state = TextState::TitleDone(p1, p2, i),
                    _ => state = TextState::Normal,
                },
                (TextState::LocationBegin(p1, p2, p3, p4), _) => {
                    if ch == ')' {
                        // when found ')', this means that we found a valid picture or url.
                        let begin = p1.unwrap_or(p2);
                        // the part of normal text before '![]()' or '[]()' mark.
                        let s = utf8_slice::slice(content, last, begin);
                        if !s.is_empty() {
                            buff.push(Token::new(s.to_string(), TokenKind::Text));
                        }
                        // '![]()' or '[]()' mark
                        let s = utf8_slice::slice(content, begin, i + 1);
                        let mut t = if p1.is_some() {
                            Token::new(s.to_string(), TokenKind::Image)
                        } else {
                            Token::new(s.to_string(), TokenKind::Url)
                        };
                        t.insert_show_name(utf8_slice::slice(content, p2 + 1, p3));
                        t.insert_location(utf8_slice::slice(content, p4 + 1, i));
                        buff.push(t);

                        last = i + 1;
                        state = TextState::Normal;
                    }
                }
                (TextState::Star(p), _) => {
                    if let Some((_, n)) = content_iter.peek() {
                        if *n != '*' {
                            let s = utf8_slice::slice(content, p, i + 1);
                            buff.push(Token::new(s.to_string(), TokenKind::StarMark));

                            last = i + 1;
                            state = TextState::Normal;
                        }
                    }
                }
            }
        }
        Self::tidy_inside(&mut buff);
        buff
    }

    // TODO: need to optimize
    fn tidy_inside(buff: &mut Vec<Token>) {
        let mut splits_at = vec![];
        let mut pre = 0;
        buff.iter().enumerate().for_each(|(ix, t)| {
            if t.kind() != TokenKind::StarMark {
                return;
            }
            let n = t.value().len() - pre;
            if pre > 0 && n > 0 {
                let l = splits_at.len();
                splits_at.push((ix + l, pre));
                pre = n;
            } else {
                pre = t.value().len();
            }
        });
        splits_at.into_iter().for_each(|(ix, at)| {
            let off = buff[ix].split_off(at);
            buff.insert(ix + 1, off);
        });

        let mut buff_iter = buff
            .iter_mut()
            .filter(|t| t.kind() == TokenKind::StarMark)
            .peekable();
        while let Some(t) = buff_iter.next() {
            match t.value() {
                "*" | "**" | "***" => {
                    if let Some(next) = buff_iter.peek() {
                        if next.value() == t.value() {
                            // update current token to correct kind
                            t.kind = match t.value() {
                                "*" => TokenKind::ItalicMark,
                                "**" => TokenKind::BoldMark,
                                "***" => TokenKind::ItalicBoldMark,
                                _ => TokenKind::StarMark,
                            };
                            // update next token to correct kind
                            // Note: here consumed the next element(a token)
                            match buff_iter.next() {
                                None => break,
                                Some(n) => n.kind = t.kind(),
                            }
                            continue;
                        }
                    }

                    t.kind = TokenKind::Text;
                }
                _ => {
                    // TODO:
                    t.kind = TokenKind::Text;
                }
            }
        }
    }

    // find 'line break', double spaces or <br> at the end of the line
    fn has_br(s: &str) -> bool {
        if s.ends_with("  \n") {
            true
        } else {
            s.trim_end().ends_with("<br>")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_ast(s: &str) -> Ast {
        let mut ast = Ast::new();
        ast.parse_string(s).unwrap();
        ast
    }

    #[test]
    fn test_parse_all() {
        let md = "# dice 是一个 markdown 编辑器
这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。

重点:
* 支持 vim 操作
* 支持自动化的命令去编辑文档
* 扩展内容的左右水平布局
* 更好交互的表格
* 支持幻灯片模式的播放
* 必须是快速的
* ...

1. 支持 vim 操作
2. 支持自动化的命令去编辑文档
3. 扩展内容的左右水平布局
4. 更好交互的表格
5. 支持幻灯片模式的播放
6. 必须是快速的
7. ...

---
****
_____________
--- 这不是一个分界线
---a 这也不是一个分界线

这里带有换行  
新的一行

![这是图片](/assets/img/philly-magic-garden.jpg \"Magic Gardens\")
这里是一个链接 [链接](https://a.com/assets/img/philly-magic-garden.jpg \"Magic Gardens\")

> Rust, A language empowering everyone to build reliable and efficient software.";

        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();
        assert_eq!(ast.count_lines(), md.lines().count());
    }

    #[test]
    fn test_establish_blocks() {
        let md = "# dice 是一个 markdown 编辑器
这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。

* 支持 vim 操作
  vim1
* 支持自动化的命令去编辑文档

1. 支持 vim 操作
2. 支持自动化的命令去编辑文档
3. 扩展内容的左右水平布局


> Rust, A language empowering everyone to build reliable and efficient software.";

        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        assert_eq!(ast.count_lines(), md.lines().count());
        assert_eq!(ast.blocks.len(), 8);

        assert_eq!(
            ast.blocks[3]
                .lines()
                .get(0)
                .unwrap()
                .borrow()
                .nesting_lines
                .len(),
            1
        );
        assert_eq!(
            ast.blocks[3]
                .lines()
                .get(0)
                .unwrap()
                .borrow()
                .nesting_blocks
                .len(),
            1
        );
    }

    #[test]
    fn test_parse_title() {
        let marks = vec!["# ", "## ", "### ", "#### "];
        let titles = vec!["Header1", "Header1 ", " Header1 Header1", " ", ""];

        for mark in marks {
            let mut n = 0;
            let mut ast = Ast::new();
            for title in titles.iter() {
                let mut s = mark.to_string();
                s += title;
                s += "\n";

                n += 1;
                ast.parse_line(n, s)
            }

            assert_eq!(ast.count_lines(), n as usize);
            {
                let l = ast.get_line(1).unwrap().borrow();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.buff,
                    vec![
                        Token::new(mark.trim_end().to_string(), TokenKind::TitleMark),
                        Token::new(titles.get(0).unwrap().trim().to_string(), TokenKind::Text,)
                    ]
                );
            }
            {
                let l = ast.get_line(2).unwrap().borrow();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.buff,
                    vec![
                        Token::new(mark.trim_end().to_string(), TokenKind::TitleMark,),
                        Token::new(titles.get(1).unwrap().trim().to_string(), TokenKind::Text,)
                    ]
                );
            }
            {
                let l = ast.get_line(3).unwrap().borrow();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.buff,
                    vec![
                        Token::new(mark.trim_end().to_string(), TokenKind::TitleMark,),
                        Token::new(titles.get(2).unwrap().to_string(), TokenKind::Text,)
                    ]
                );
            }
            {
                let l = ast.get_line(4).unwrap().borrow();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.buff,
                    vec![Token::new(
                        mark.trim_end().to_string(),
                        TokenKind::TitleMark
                    )]
                );
            }
            {
                let l = ast.get_line(5).unwrap().borrow();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.buff,
                    vec![Token::new(
                        mark.trim_end().to_string(),
                        TokenKind::TitleMark
                    )]
                );
            }
        }
    }

    #[test]
    fn test_parse_title2() {
        let title = "# header1 [这是一个链接](https://example.com)";
        let ast = create_ast(title);

        assert_eq!(ast.count_lines(), 1);
        assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::Title);

        let mut kvs = HashMap::new();
        kvs.insert("show_name".to_string(), "这是一个链接".to_string());
        kvs.insert("location".to_string(), "https://example.com".to_string());

        let mut url = Token::new(
            "[这是一个链接](https://example.com)".to_string(),
            TokenKind::Url,
        );
        url.details = Some(kvs);
        assert_eq!(
            ast.get_line(1).unwrap().borrow().buff,
            vec![
                Token::new("#".to_string(), TokenKind::TitleMark),
                Token::new("header1 ".to_string(), TokenKind::Text),
                url,
            ]
        );
    }

    #[test]
    fn test_parse_dividing() {
        let marks = vec![
            "---",
            "***",
            "___",
            "------",
            "****",
            "__________         ",
            "----------------------------------------   ",
        ];

        for mark in marks {
            let ast = create_ast(mark);

            assert_eq!(ast.count_lines(), 1);
            assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::DividingLine);
            assert_eq!(
                ast.get_line(1).unwrap().borrow().buff,
                vec![Token::new(
                    mark.trim_end().to_string(),
                    TokenKind::DividingMark,
                )]
            );
        }
    }

    #[test]
    fn test_parse_normal() {
        {
            let contents = vec![
                "这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。",
                "--- x",
                "___ 这不是一个分界线",
                "--- ---",
                "___ ___",
                "#这不是标题",
                "##这也不是标题",
                ">这不是引用",
                "1.这也不是列表",
            ];

            for cnt in contents {
                let ast = create_ast(cnt);

                assert_eq!(ast.count_lines(), 1);
                assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
                assert_eq!(
                    ast.get_line(1).unwrap().borrow().buff,
                    vec![Token::new(cnt.to_string(), TokenKind::Text)]
                );
            }
        }
        {
            let contents = vec!["***xxxx"];

            for cnt in contents {
                let ast = create_ast(cnt);

                assert_eq!(ast.count_lines(), 1);
                assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
                assert_eq!(
                    ast.get_line(1).unwrap().borrow().buff,
                    vec![
                        Token::new("***".to_string(), TokenKind::Text),
                        Token::new("xxxx".to_string(), TokenKind::Text)
                    ]
                );
            }
        }
        {
            let contents = vec!["**1**"];

            for cnt in contents {
                let ast = create_ast(cnt);

                assert_eq!(ast.count_lines(), 1);
                assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
                assert_eq!(
                    ast.get_line(1).unwrap().borrow().buff,
                    vec![
                        Token::new("**".to_string(), TokenKind::BoldMark),
                        Token::new("1".to_string(), TokenKind::Text,),
                        Token::new("**".to_string(), TokenKind::BoldMark),
                    ]
                );
            }
        }
        {
            let cnt = "*1*";
            let ast = create_ast(cnt);

            assert_eq!(ast.count_lines(), 1);
            assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
            assert_eq!(
                ast.get_line(1).unwrap().borrow().buff,
                vec![
                    Token::new("*".to_string(), TokenKind::ItalicMark),
                    Token::new("1".to_string(), TokenKind::Text,),
                    Token::new("*".to_string(), TokenKind::ItalicMark),
                ]
            );
        }
        {
            let cnt = "***1***";
            let ast = create_ast(cnt);

            assert_eq!(ast.count_lines(), 1);
            assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
            assert_eq!(
                ast.get_line(1).unwrap().borrow().buff,
                vec![
                    Token::new("***".to_string(), TokenKind::ItalicBoldMark),
                    Token::new("1".to_string(), TokenKind::Text,),
                    Token::new("***".to_string(), TokenKind::ItalicBoldMark),
                ]
            );
        }
    }

    #[test]
    fn test_parse_linebreak() {
        let contents = vec![
            "这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。  ",  // have two spaces at the end of the line.
            "这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。       ", // have two tab spaces at the end of the line.
            "这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。    <br>  ", // have two spaces at the end of the line.
            "这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。<br>",
        ];

        for cnt in contents {
            let ast = create_ast(cnt);

            assert_eq!(ast.count_lines(), 1);
            assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
            assert_eq!(
                ast.get_line(1).unwrap().borrow().buff,
                vec![
                    Token::new(
                        cnt.trim().trim_end_matches("<br>").to_string(),
                        TokenKind::Text
                    ),
                    Token::new("<br>".to_string(), TokenKind::LineBreak)
                ]
            );
        }
    }

    #[test]
    fn test_parse_quote() {
        let mark = ">";
        let content =
            "Rust, A language empowering everyone to build reliable and efficient software.";

        let mut s = mark.to_string();
        s.push_str(" ");
        s.push_str(content);

        let ast = create_ast(&s);

        assert_eq!(ast.count_lines(), 1);
        assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::Quote);
        assert_eq!(
            ast.get_line(1).unwrap().borrow().buff,
            vec![
                Token::new(mark.to_string(), TokenKind::QuoteMark),
                Token::new(content.to_string(), TokenKind::Text)
            ]
        );
    }

    #[test]
    fn test_parse_disorder_list() {
        let mark = "*";
        let content =
            "Rust, A language empowering everyone to build reliable and efficient software.";

        let mut s = mark.to_string();
        s.push_str(" ");
        s.push_str(content);

        let ast = create_ast(&s);

        assert_eq!(ast.count_lines(), 1);
        assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::DisorderedList);
        assert_eq!(
            ast.get_line(1).unwrap().borrow().buff,
            vec![
                Token::new(mark.to_string(), TokenKind::DisorderListMark),
                Token::new(content.to_string(), TokenKind::Text,)
            ]
        );
    }

    #[test]
    fn test_parse_sorted_list() {
        let marks = vec!["1.", "2.", "3.", "4.", "10.", "11.", "99.", "100.", "999."];
        let content =
            "Rust, A language empowering everyone to build reliable and efficient software.";

        for mark in marks {
            let mut s = mark.to_string();
            s.push_str(" ");
            s.push_str(content);
            let ast = create_ast(&s);

            assert_eq!(ast.count_lines(), 1);
            assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::SortedList);
            assert_eq!(
                ast.get_line(1).unwrap().borrow().buff,
                vec![
                    Token::new(mark.to_string(), TokenKind::SortedListMark),
                    Token::new(content.to_string(), TokenKind::Text,)
                ]
            );
        }
    }

    #[test]
    fn test_blank_line() {
        let contents = vec![
            "\n",
            " \n",
            "     \n",
            "         ",
            "                                            ",
            "  ",
        ];

        for cnt in contents {
            let ast = create_ast(cnt);

            assert_eq!(ast.count_lines(), 1);
            assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::Blank);
            assert_eq!(
                ast.get_line(1).unwrap().borrow().buff,
                vec![Token::new(
                    cnt.trim_end_matches('\n').to_string(),
                    TokenKind::BlankLine
                )]
            );
        }
    }

    #[test]
    fn test_picture() {
        let pics = vec![
            "![这是图片](/assets/img/philly-magic-garden.jpg \"Magic Gardens\")",
            "![](/assets/img/philly-magic-garden.jpg \"Magic Gardens\")",
            "![]()",
            "![[[[[[]()",
            "![[]]()",
            "![!]()",
            "![![]]()",
            // "![![]()]()",
        ];

        for pic in pics {
            let contents = vec!["text", "text ", "text !", "text !!!!", ""];
            for cnt in contents {
                let s = cnt.to_string() + pic;
                let ast = create_ast(&s);

                assert_eq!(ast.count_lines(), 1);
                assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
                if cnt.is_empty() {
                    // token 0
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].kind,
                        TokenKind::Image
                    );
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].value,
                        pic.to_string()
                    );
                } else {
                    // token 0
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].kind,
                        TokenKind::Text
                    );
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].value,
                        cnt.to_string()
                    );

                    // token 1
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[1].kind,
                        TokenKind::Image
                    );
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[1].value,
                        pic.to_string()
                    );
                }
            }
        }
    }

    #[test]
    fn test_url() {
        let urls = vec![
            "[这是链接](/assets/img/philly-magic-garden.jpg \"Magic Gardens\")",
            "[](/assets/img/philly-magic-garden.jpg \"Magic Gardens\")",
            "[]()",
            "[]]()",
            "[]]]]()",
            "[!]]()",
        ];

        for url in urls {
            let contents = vec!["text", "text ", "text [", "text [[[[", ""];
            for cnt in contents {
                let s = cnt.to_string() + url;
                let ast = create_ast(&s);

                assert_eq!(ast.count_lines(), 1);
                assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::NormalText);
                if cnt.is_empty() {
                    // token 0
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].kind,
                        TokenKind::Url
                    );
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].value,
                        url.to_string()
                    );
                } else {
                    // token 0
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].kind,
                        TokenKind::Text
                    );
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[0].value,
                        cnt.to_string()
                    );

                    // token 1
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[1].kind,
                        TokenKind::Url
                    );
                    assert_eq!(
                        ast.get_line(1).unwrap().borrow().buff[1].value,
                        url.to_string()
                    );
                }
            }
        }
    }

    #[test]
    fn test_code_mark() {
        let mut ast = Ast::new();
        ast.parse_string("``` ").unwrap();

        assert_eq!(ast.count_lines(), 1);
        assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::CodeMark);
        assert_eq!(
            ast.get_line(1).unwrap().borrow().buff,
            vec![Token::new("```".to_string(), TokenKind::CodeMark)]
        );

        let mut ast = Ast::new();
        ast.parse_string("```rust").unwrap();

        assert_eq!(ast.count_lines(), 1);
        assert_eq!(ast.get_line(1).unwrap().borrow().kind, Kind::CodeMark);
        assert_eq!(
            ast.get_line(1).unwrap().borrow().buff,
            vec![
                Token::new("```".to_string(), TokenKind::CodeMark),
                Token::new("rust".to_string(), TokenKind::Text)
            ]
        );
    }

    #[test]
    fn test_code_block() {
        let md = "这是一个代码块的例子：
```
    let s = \"hello world\";
    let s1 = s.to_string();
```
    ```
    let s;
    assert_eq!(ast.doc[0].sequence[1].kind, TokenKind::Url);
    assert_eq!(ast.doc[0].sequence[1].value, url.to_string());";

        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        assert_eq!(ast.count_lines(), md.lines().count());
        assert_eq!(ast.blocks.len(), 6);

        assert_eq!(ast.blocks[1].kind(), Kind::Code);
        assert_eq!(ast.blocks[1]._count(), 4);

        assert_eq!(ast.blocks[2].kind(), Kind::CodeMark);
        assert_eq!(ast.blocks[2]._count(), 1);

        assert_eq!(ast.blocks[5].kind(), Kind::NormalText);
        assert_eq!(ast.blocks[5]._count(), 1);
    }
}
