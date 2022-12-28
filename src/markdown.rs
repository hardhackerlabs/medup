use std::error::Error;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::{fmt, io};

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
struct Ast {
    doc: Vec<Line>,
    blocks: Vec<Block>,
    enable_defer_parse: bool,
    defer_queue: Vec<usize>, // usize is index to 'doc'
}

impl Ast {
    // Create a Ast instance.
    fn new() -> Self {
        Ast {
            doc: Vec::new(),
            blocks: Vec::new(),
            defer_queue: Vec::new(),
            enable_defer_parse: false,
        }
    }

    // Parse markdown document from a file, the 'path' argument is the file path.
    fn parse_file(&mut self, path: &str) -> Result<(), io::Error> {
        let file = File::open(path)?;
        self.parse_reader(&mut BufReader::new(file))
    }

    // Parse markdown document from a string.
    fn parse_string(&mut self, s: &str) -> Result<(), io::Error> {
        self.parse_reader(&mut s.as_bytes())
    }

    // Parse markdown document from a reader, the 'reader' may be a file reader, byte buff or network socket etc.
    fn parse_reader(&mut self, reader: &mut dyn BufRead) -> Result<(), io::Error> {
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
        for i in self.defer_queue.iter() {
            self.doc[*i].parse()
        }
        self.defer_queue.clear();
        self.build_blocks();
        Ok(())
    }

    fn parse_line(&mut self, num: usize, line: String) {
        let l = if self.enable_defer_parse && Self::peek_not(&line, "```") {
            self.defer_queue.push(self.doc.len());
            Line::new_without_parsing(num, line)
        } else {
            Line::new(num, line)
        };
        if l.kind == Kind::CodeMark {
            self.enable_defer_parse = !self.enable_defer_parse;
            if !self.enable_defer_parse {
                // closed code block
                self.defer_queue.clear();
            }
        }
        self.doc.push(l);
    }

    fn build_blocks(&mut self) {
        for (i, l) in self.doc.iter().enumerate() {
            match l.kind {
                Kind::Blank
                | Kind::DisorderedList
                | Kind::SortedList
                | Kind::Quote
                | Kind::Code => {
                    let last = self.blocks.last_mut();
                    if let Some(b) = last {
                        if b.kind == l.kind {
                            b.line_pointers.push(i);
                            continue;
                        }
                    }
                    self.blocks.push(Block {
                        line_pointers: vec![i],
                        kind: l.kind,
                    });
                }
                _ => {
                    self.blocks.push(Block {
                        line_pointers: vec![i],
                        kind: l.kind,
                    });
                }
            }
        }
    }

    fn for_each_block(
        &self,
        out: &mut Vec<String>,
        get: fn(&mut Vec<String>, Kind, Vec<&Line>) -> Result<(), Box<dyn Error>>,
    ) -> Result<(), Box<dyn Error>> {
        for b in &self.blocks {
            let mut ls = vec![];
            for i in &b.line_pointers {
                if let Some(l) = self.doc.get(*i) {
                    ls.push(l);
                }
            }
            get(out, b.kind, ls)?;
        }
        Ok(())
    }

    fn peek_not(s1: &str, s2: &str) -> bool {
        s1.trim() != s2
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
        for line in &self.doc {
            debug.push_str(format!("[{}, {:?}]: ", line.num, line.kind).as_str());
            for t in &line.sequence {
                let s = format!("{:?} ", t);
                debug.push_str(&s);
            }
            debug.push('\n');
        }
        writeln!(f, "{}", debug)
    }
}

// Block is a group of multiple lines.
struct Block {
    line_pointers: Vec<usize>, // it stores the index of 'Line' struct in 'ast.doc'
    kind: Kind,
}

// Line is a line of the markdown file, it be parsed into some tokens.
struct Line {
    sequence: Vec<Token>,
    kind: Kind,
    num: usize,
    text: String,
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
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum TokenKind {
    TitleMark,        // #, ##, ###, ####
    DisorderListMark, // *
    SortedListMark,   // 1.
    DividingMark,     // ---, ***, ___
    QuoteMark,        // >
    BoldMark,         // ** **
    CodeMark,         // ```
    BlankLine,        // \n
    LineBreak,        // <br>, double whitespace
    Picture,          // ![]()
    Url,              // []()
    Text,             //
}

impl Line {
    fn new(ln: usize, line: String) -> Self {
        let mut l = Line {
            num: ln,
            text: line,
            kind: Kind::NormalText,
            sequence: Vec::new(),
        };
        l.parse();
        l
    }
    fn new_without_parsing(ln: usize, line: String) -> Self {
        Line {
            num: ln,
            text: line,
            kind: Kind::Code,
            sequence: Vec::new(),
        }
    }

    // Parses a line of text into 'Line' struct that contains multi tokens.
    // Line's kind is determinded by the first token's kind.
    fn parse(&mut self) {
        let mut lx = Lexer::new(&self.text, &mut self.sequence);

        for (cur_pos, cur_char) in self.text.chars().enumerate() {
            let stopped = lx.process(cur_pos, cur_char);
            if stopped {
                break;
            }
        }
        lx.finish();

        let calc_kind = |kind| match kind {
            TokenKind::BlankLine => Kind::Blank,
            TokenKind::TitleMark => Kind::Title,
            TokenKind::DisorderListMark => Kind::DisorderedList,
            TokenKind::SortedListMark => Kind::SortedList,
            TokenKind::DividingMark => Kind::DividingLine,
            TokenKind::QuoteMark => Kind::Quote,
            TokenKind::CodeMark => Kind::CodeMark,
            _ => Kind::NormalText,
        };

        self.kind = match self.sequence.first() {
            None => Kind::NormalText,
            Some(t) => calc_kind(t.kind),
        }
    }
}

// Token is a part of the line, the parser will parse the line into some tokens.
#[derive(PartialEq, Debug)]
struct Token {
    value: String,
    kind: TokenKind,
    parent_kind: Option<TokenKind>,
    fields: Option<Vec<String>>,
}

impl Token {
    fn new(value: String, kind: TokenKind) -> Self {
        Token {
            value,
            kind,
            parent_kind: None,
            fields: None,
        }
    }
    fn new_with_parent(value: String, kind: TokenKind, parent: TokenKind) -> Self {
        Token {
            value,
            kind,
            parent_kind: Some(parent),
            fields: None,
        }
    }
    fn push_field(&mut self, s: &str) {
        self.fields.get_or_insert(vec![]).push(s.to_string())
    }
}

// Lexer used to split the text of the line into some tokens,
// It is implemented with a state machine.
struct Lexer<'lex> {
    state: State,
    unparsed: usize,
    line_tokens: &'lex mut Vec<Token>,
    line_text: &'lex str,
}

#[derive(PartialEq)]
enum State {
    Begin,
    Mark,
    Title,
    DisorderedList,
    SortedList,
    Quote,
    VerifyDividing,
    VerifyCodeMark,
    Normal,
    Finished,
}

#[derive(PartialEq, Clone, Copy)]
enum TextState {
    Normal,
    // usize is start position in line
    Bold(usize),
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

impl<'lex> Lexer<'lex> {
    fn new(text: &'lex str, tokens: &'lex mut Vec<Token>) -> Self {
        Lexer {
            state: State::Begin,
            unparsed: 0,
            line_text: text,
            line_tokens: tokens,
        }
    }

    fn process(&mut self, cur_pos: usize, cur_char: char) -> bool {
        match self.state {
            State::Begin => {
                if let Some(t) = self.split_prefix_whitespaces(cur_pos, cur_char) {
                    self.line_tokens.push(t)
                }
            }
            State::Mark => {
                if let Some(t) = self.split_mark(cur_pos) {
                    self.line_tokens.push(t)
                }
            }
            State::Title => {
                if let Some(t) = self.split_title(cur_pos, cur_char) {
                    self.line_tokens.push(t)
                }
            }
            State::DisorderedList => {
                if let Some(t) =
                    self.split_list_item(cur_pos, cur_char, TokenKind::DisorderListMark)
                {
                    self.line_tokens.push(t)
                }
            }
            State::SortedList => {
                if let Some(t) = self.split_list_item(cur_pos, cur_char, TokenKind::SortedListMark)
                {
                    self.line_tokens.push(t)
                }
            }
            State::VerifyDividing => {
                self.verify_dividing(cur_pos, cur_char);
            }
            State::VerifyCodeMark => {
                self.verify_codemark(cur_pos, cur_char);
            }
            State::Quote => {
                if let Some(t) = self.split_quote(cur_pos, cur_char) {
                    self.line_tokens.push(t)
                }
            }
            State::Normal => {
                if let Some(ts) = self.split_normal_text() {
                    for t in ts {
                        if t.value.is_empty() {
                            continue;
                        }
                        self.line_tokens.push(t);
                    }
                }
            }
            State::Finished => {}
        };

        self.state == State::Finished
    }

    fn finish(&mut self) {
        let len = utf8_slice::len(self.line_text);
        if self.unparsed < len {
            self.process(len - 1, '\n');
        }
    }

    fn set_state(&mut self, last: usize, state: State) {
        self.unparsed = last;
        self.state = state;
    }

    fn end(&mut self) {
        self.set_state(utf8_slice::len(self.line_text), State::Finished);
    }

    // skip all whitespace characters at the beginning of the line,
    // or generate a blank line token if the line contains only whitespace characters.
    fn split_prefix_whitespaces(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            if cur_char == '\n' {
                self.end();
                return Some(Token::new("".to_string(), TokenKind::BlankLine));
            }
            return None;
        }
        self.set_state(cur_pos, State::Mark);
        None
    }

    fn find_word(&self, cur_pos: usize) -> Option<&str> {
        if self.line_text.chars().nth(cur_pos)?.is_whitespace() {
            Some(utf8_slice::slice(self.line_text, self.unparsed, cur_pos))
        } else {
            None
        }
    }

    // parse the first word in the line as the mark token
    fn split_mark(&mut self, cur_pos: usize) -> Option<Token> {
        let first_word = self.find_word(cur_pos)?;
        let first_word_chars: Vec<char> = first_word.chars().collect();

        let (unparsed, state, token) = match first_word_chars[..] {
            // title
            ['#'] | ['#', '#'] | ['#', '#', '#'] | ['#', '#', '#', '#'] => (
                cur_pos + 1,
                State::Title,
                Some(Token::new(first_word.to_string(), TokenKind::TitleMark)),
            ),
            // disordered list
            ['*'] | ['-'] | ['+'] => (
                cur_pos + 1,
                State::DisorderedList,
                Some(Token::new(
                    first_word.to_string(),
                    TokenKind::DisorderListMark,
                )),
            ),
            // sorted list
            [n1, '.'] if ('1'..='9').contains(&n1) => (
                cur_pos + 1,
                State::SortedList,
                Some(Token::new(
                    first_word.to_string(),
                    TokenKind::SortedListMark,
                )),
            ),
            [n1, n2, '.'] if ('1'..='9').contains(&n1) && ('0'..='9').contains(&n2) => (
                cur_pos + 1,
                State::SortedList,
                Some(Token::new(
                    first_word.to_string(),
                    TokenKind::SortedListMark,
                )),
            ),
            [n1, n2, n3, '.']
                if ('1'..='9').contains(&n1)
                    && ('0'..='9').contains(&n2)
                    && ('0'..='9').contains(&n3) =>
            {
                (
                    cur_pos + 1,
                    State::SortedList,
                    Some(Token::new(
                        first_word.to_string(),
                        TokenKind::SortedListMark,
                    )),
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
                    State::VerifyDividing,
                    Some(Token::new(first_word.to_string(), TokenKind::DividingMark)),
                )
            }
            // quote
            ['>'] => (
                cur_pos + 1,
                State::Quote,
                Some(Token::new(first_word.to_string(), TokenKind::QuoteMark)),
            ),
            // code block mark
            ['`', '`', '`'] => (
                cur_pos + 1,
                State::VerifyCodeMark,
                Some(Token::new(first_word.to_string(), TokenKind::CodeMark)),
            ),
            // normal (as no mark)
            _ => {
                // don't change the unparsed pointer, because the first word is not a mark.
                (self.unparsed, State::Normal, None)
            }
        };

        self.set_state(unparsed, state);
        token
    }

    // if there is not a valid dividing line, will recreate the token as normal.
    fn verify_dividing(&mut self, _cur_pos: usize, cur_char: char) -> Option<Token> {
        if !cur_char.is_whitespace() {
            // if contains other characters, it's a invalid dividing line
            // not a valid dividing line, so clear the dividing mark token
            self.line_tokens.clear();
            self.set_state(0, State::Normal);
        }
        None
    }

    //
    fn verify_codemark(&mut self, _cur_pos: usize, cur_char: char) -> Option<Token> {
        if !cur_char.is_whitespace() {
            self.line_tokens.clear();
            self.set_state(0, State::Normal);
        }
        None
    }

    // parse the rest of the line as title token
    fn split_title(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.end();
        Some(Token::new_with_parent(
            rest.trim_end().to_string(),
            TokenKind::Text,
            TokenKind::TitleMark,
        ))
    }

    // parse the rest of the line as a list item token.
    fn split_list_item(
        &mut self,
        cur_pos: usize,
        cur_char: char,
        parent: TokenKind,
    ) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.end();
        Some(Token::new_with_parent(
            rest.trim_end().to_string(),
            TokenKind::Text,
            parent,
        ))
    }

    // parse the rest of the line as the quote token.
    fn split_quote(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.end();
        Some(Token::new_with_parent(
            rest.trim_end().to_string(),
            TokenKind::Text,
            TokenKind::QuoteMark,
        ))
    }

    // parse the line as the normal token.
    fn split_normal_text(&mut self) -> Option<Vec<Token>> {
        let mut buff = Self::parse_inside(utf8_slice::from(self.line_text, self.unparsed));
        if Self::has_line_break(self.line_text) {
            buff.push(Token::new("<br>".to_string(), TokenKind::LineBreak));
        }
        self.end();
        Some(buff)
    }

    // parse text content, include bold, picture and url etc.
    fn parse_inside(content: &str) -> Vec<Token> {
        let mut last = 0;

        let mut buff: Vec<Token> = Vec::new();
        let mut state = TextState::Normal;

        for (i, ch) in content.chars().enumerate() {
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
                    '*' => state = TextState::Bold(i),
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

                        let begin = if let Some(p1) = p1 { p1 } else { p2 };
                        // the part of normal text before '![]()' or '[]()' mark.
                        let s = utf8_slice::slice(content, last, begin);
                        if !s.is_empty() {
                            buff.push(Token::new(s.to_string(), TokenKind::Text));
                        }
                        // '![]()' or '[]()' mark
                        let s = utf8_slice::slice(content, begin, i + 1);
                        let mut t = if p1.is_some() {
                            Token::new(s.to_string(), TokenKind::Picture)
                        } else {
                            Token::new(s.to_string(), TokenKind::Url)
                        };
                        t.push_field(utf8_slice::slice(content, p2 + 1, p3));
                        t.push_field(utf8_slice::slice(content, p4 + 1, i));
                        buff.push(t);

                        last = i + 1;
                        state = TextState::Normal;
                    }
                }
                (TextState::Bold(p), _) => match ch {
                    // so it's '**'.
                    '*' => {
                        // the part of normal text before '**' mark.
                        let s = utf8_slice::slice(content, last, p);
                        if !s.is_empty() {
                            buff.push(Token::new(s.to_string(), TokenKind::Text));
                        }
                        // '**' mark.
                        buff.push(Token::new("**".to_string(), TokenKind::BoldMark));

                        last = i + 1;
                        state = TextState::Normal;
                    }
                    _ => state = TextState::Normal,
                },
            }
        }
        Self::tidy_inside_tokens(&mut buff);
        buff
    }

    fn tidy_inside_tokens(buff: &mut [Token]) {
        let mut tmp: Vec<(usize, usize)> = Vec::new();
        let mut in_bold = false;
        let mut begin = 0;

        for (i, t) in buff.iter().enumerate() {
            if t.kind == TokenKind::BoldMark {
                if !in_bold {
                    begin = i;
                } else {
                    tmp.push((begin, i));
                }
                in_bold = !in_bold;
            }
        }
        // the bold not be closed, so change kind of the bold mark to text.
        if in_bold {
            let t = &mut buff[begin];
            t.kind = TokenKind::Text;
        }

        // in bold
        for (b, e) in tmp {
            for t in buff[b..=e].iter_mut() {
                if t.kind == TokenKind::BoldMark {
                    continue;
                }
                t.parent_kind = Some(TokenKind::BoldMark);
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

// The html module is used to parse markdown into html.
pub mod html {
    use serde::Serialize;

    use std::error::Error;
    use tinytemplate::TinyTemplate;

    use super::Line;

    // Title
    static TITLE_TEMPLATE: &str = "{{ if is_l1 }}<h1>{text}</h1>{{ endif }}
    {{ if is_l2 }}<h2>{text}</h2>{{ endif }}
    {{ if is_l3 }}<h3>{text}</h3>{{ endif }}
    {{ if is_l4 }}<h4>{text}</h4>{{ endif }}";

    #[derive(Serialize)]
    struct TitleContext<'tc> {
        is_l1: bool,
        is_l2: bool,
        is_l3: bool,
        is_l4: bool,
        text: &'tc str,
    }

    fn render_title(l: &Line) -> Result<String, Box<dyn Error>> {
        let first = &l.sequence[0];
        let level = first.value.len();
        let second = l.sequence.get(1);

        let ctx = TitleContext {
            is_l1: level == 1,
            is_l2: level == 2,
            is_l3: level == 3,
            is_l4: level == 4,
            text: if let Some(t) = second {
                t.value.as_str()
            } else {
                ""
            },
        };

        let mut tt = TinyTemplate::new();
        tt.add_template("title", TITLE_TEMPLATE)?;
        let s = tt.render("title", &ctx)?;
        Ok(s)
    }

    // Sorted List
    static SORTED_LIST_TEMPLATE: &str = "<ol> 
    {{ for item in list }} 
    <li>{item}</li> 
    {{ endfor }} 
    </ol>";

    #[derive(Serialize)]
    struct SortedListContext<'slc> {
        list: Vec<&'slc str>,
    }

    fn render_sorted_list(ls: Vec<&Line>) -> Result<String, Box<dyn Error>> {
        let mut list = Vec::new();
        for l in ls {
            list.push(if let Some(t) = l.sequence.get(1) {
                t.value.as_str()
            } else {
                ""
            });
        }
        let ctx = SortedListContext { list };
        let mut tt = TinyTemplate::new();
        tt.add_template("sorted_list", SORTED_LIST_TEMPLATE)?;
        let s = tt.render("sorted_list", &ctx)?;
        Ok(s)
    }

    // Disordered List
    static DISORDERED_LIST_TEMPLATE: &str = "<ul> 
    {{ for item in list }} 
    <li>{item}</li> 
    {{ endfor }} 
    </ul>";

    #[derive(Serialize)]
    struct DisorderedListContext<'dlc> {
        list: Vec<&'dlc str>,
    }

    fn render_disordered_list(ls: Vec<&Line>) -> Result<String, Box<dyn Error>> {
        let mut list = Vec::new();
        for l in ls {
            list.push(if let Some(t) = l.sequence.get(1) {
                t.value.as_str()
            } else {
                ""
            });
        }
        let ctx = DisorderedListContext { list };
        let mut tt = TinyTemplate::new();
        tt.add_template("disorded_list", DISORDERED_LIST_TEMPLATE)?;
        let s = tt.render("disorded_list", &ctx)?;
        Ok(s)
    }

    // Normal Text
    fn render_normal(ls: Vec<&Line>) -> Result<String, Box<dyn Error>> {
        let mut s = String::new();
        for l in ls {
            let mut text = String::new();
            let mut in_bold = false;
            for t in &l.sequence {
                match t.kind {
                    super::TokenKind::Text => text.push_str(&t.value),
                    super::TokenKind::BoldMark => {
                        if in_bold {
                            text.push_str("</strong>");
                        } else {
                            text.push_str("<strong>");
                        }
                        in_bold = !in_bold;
                    }
                    super::TokenKind::Url => {
                        if let Some(fields) = &t.fields {
                            if fields.len() == 2 {
                                let s = render_url(&fields[0], &fields[1])?;
                                text.push_str(&s);
                            }
                        }
                    }
                    super::TokenKind::Picture => {} // TODO:
                    _ => (),
                }
            }
            s.push_str("<p>");
            s.push_str(&text);
            s.push_str("</p>");
        }
        Ok(s)
    }

    // Quote
    static QUOTE_TEMPLATE: &str = "<blockquote> 
    {{ for text in lines }} 
    {text}<br> 
    {{ endfor }} 
    </blockquote>";

    #[derive(Serialize)]
    struct QuoteContext<'qc> {
        lines: Vec<&'qc str>,
    }

    fn render_quote(ls: Vec<&Line>) -> Result<String, Box<dyn Error>> {
        let mut lines = Vec::new();
        for l in ls {
            lines.push(if let Some(t) = l.sequence.get(1) {
                t.value.as_str()
            } else {
                ""
            });
        }
        let mut tt = TinyTemplate::new();
        tt.add_template("quote", QUOTE_TEMPLATE)?;
        let s = tt.render("quote", &QuoteContext { lines })?;
        Ok(s)
    }

    // Dividling
    fn render_dividling() -> Result<String, Box<dyn Error>> {
        Ok("<hr>".to_string())
    }

    #[derive(Serialize)]
    struct CodeBlockContext {}

    fn render_code() -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    // Url
    static URL_TEMPLATE: &str = "<a href=\"{location}\">{title}</a>";

    #[derive(Serialize)]
    struct UrlContext<'uc> {
        title: &'uc str,
        location: &'uc str,
    }

    fn render_url(title: &str, location: &str) -> Result<String, Box<dyn Error>> {
        let mut tt = TinyTemplate::new();
        tt.add_template("url", URL_TEMPLATE)?;
        let s = tt.render("url", &UrlContext { title, location })?;
        Ok(s)
    }

    // Image

    #[derive(Serialize)]
    struct ImageContext {}

    fn render_image() -> Result<(), Box<dyn Error>> {
        Ok(())
    }

    pub mod render {
        use crate::markdown::{Ast, Kind};
        use std::error::Error;

        pub fn handle_file(path: &str) -> Result<String, Box<dyn Error>> {
            let mut ast = Ast::new();
            ast.parse_file(path)?;
            handle(&ast)
        }

        pub fn handle_string(s: &str) -> Result<String, Box<dyn Error>> {
            let mut ast = Ast::new();
            ast.parse_string(s)?;
            handle(&ast)
        }

        fn handle(ast: &Ast) -> Result<String, Box<dyn Error>> {
            let mut html_doc: Vec<String> = Vec::new();

            ast.for_each_block(&mut html_doc, |out, kind, ls| {
                let res = match kind {
                    Kind::Title => {
                        if let Some(l) = ls.first() {
                            Some(super::render_title(l)?)
                        } else {
                            None
                        }
                    }
                    Kind::CodeMark => None,
                    Kind::Code => None,
                    Kind::DisorderedList => Some(super::render_disordered_list(ls)?),
                    Kind::DividingLine => Some(super::render_dividling()?),
                    Kind::Blank | Kind::NormalText => Some(super::render_normal(ls)?),
                    Kind::Quote => Some(super::render_quote(ls)?),
                    Kind::SortedList => Some(super::render_sorted_list(ls)?),
                };
                if let Some(res) = res {
                    out.push(res);
                }
                Ok(())
            })?;

            Ok(html_doc.join("\n"))
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
        assert_eq!(ast.doc.len(), md.lines().count());
    }

    #[test]
    fn test_build_blocks() {
        let md = "# dice 是一个 markdown 编辑器
这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。

* 支持 vim 操作
* 支持自动化的命令去编辑文档

1. 支持 vim 操作
2. 支持自动化的命令去编辑文档
3. 扩展内容的左右水平布局


> Rust, A language empowering everyone to build reliable and efficient software.";

        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        assert_eq!(ast.doc.len(), md.lines().count());
        assert_eq!(ast.blocks.len(), 8);
    }

    #[test]
    fn test_parse_title() {
        let marks = vec!["#", "##", "###", "####"];
        let titles = vec![" Header1", " Header1   ", "  Header1 Header1", " ", ""];

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

            assert_eq!(ast.doc.len(), n as usize);
            {
                let l = ast.doc.get(0).unwrap();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.sequence,
                    vec![
                        Token::new(mark.to_string(), TokenKind::TitleMark),
                        Token::new_with_parent(
                            titles.get(0).unwrap().trim().to_string(),
                            TokenKind::Text,
                            TokenKind::TitleMark
                        )
                    ]
                );
            }
            {
                let l = ast.doc.get(1).unwrap();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.sequence,
                    vec![
                        Token::new(mark.to_string(), TokenKind::TitleMark,),
                        Token::new_with_parent(
                            titles.get(1).unwrap().trim().to_string(),
                            TokenKind::Text,
                            TokenKind::TitleMark
                        )
                    ]
                );
            }
            {
                let l = ast.doc.get(2).unwrap();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.sequence,
                    vec![
                        Token::new(mark.to_string(), TokenKind::TitleMark,),
                        Token::new_with_parent(
                            titles.get(2).unwrap().trim().to_string(),
                            TokenKind::Text,
                            TokenKind::TitleMark
                        )
                    ]
                );
            }
            {
                let l = ast.doc.get(3).unwrap();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.sequence,
                    vec![Token::new(mark.to_string(), TokenKind::TitleMark)]
                );
            }
            {
                let l = ast.doc.get(4).unwrap();
                assert_eq!(l.kind, Kind::Title);
                assert_eq!(
                    l.sequence,
                    vec![Token::new(mark.to_string(), TokenKind::TitleMark)]
                );
            }
        }
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

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, Kind::DividingLine);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![Token::new(
                    mark.trim_end().to_string(),
                    TokenKind::DividingMark,
                )]
            )
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
                "*这不是列表",
                "1.这也不是列表",
            ];

            for cnt in contents {
                let ast = create_ast(cnt);

                assert_eq!(ast.doc.len(), 1);
                assert_eq!(ast.doc.get(0).unwrap().kind, Kind::NormalText);
                assert_eq!(
                    ast.doc.get(0).unwrap().sequence,
                    vec![Token::new(cnt.to_string(), TokenKind::Text)]
                )
            }
        }
        {
            let contents = vec!["***xxxx"];

            for cnt in contents {
                let ast = create_ast(cnt);

                assert_eq!(ast.doc.len(), 1);
                assert_eq!(ast.doc.get(0).unwrap().kind, Kind::NormalText);
                assert_eq!(
                    ast.doc.get(0).unwrap().sequence,
                    vec![
                        Token::new("**".to_string(), TokenKind::Text),
                        Token::new("*xxxx".to_string(), TokenKind::Text)
                    ]
                )
            }
        }
        {
            let contents = vec!["**1**"];

            for cnt in contents {
                let ast = create_ast(cnt);

                assert_eq!(ast.doc.len(), 1);
                assert_eq!(ast.doc.get(0).unwrap().kind, Kind::NormalText);
                assert_eq!(
                    ast.doc.get(0).unwrap().sequence,
                    vec![
                        Token::new("**".to_string(), TokenKind::BoldMark),
                        Token::new_with_parent(
                            "1".to_string(),
                            TokenKind::Text,
                            TokenKind::BoldMark
                        ),
                        Token::new("**".to_string(), TokenKind::BoldMark),
                    ]
                )
            }
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

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, Kind::NormalText);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![
                    Token::new(
                        cnt.trim().trim_end_matches("<br>").to_string(),
                        TokenKind::Text
                    ),
                    Token::new("<br>".to_string(), TokenKind::LineBreak)
                ]
            )
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

        assert_eq!(ast.doc.len(), 1);
        assert_eq!(ast.doc[0].kind, Kind::Quote);
        assert_eq!(
            ast.doc[0].sequence,
            vec![
                Token::new(mark.to_string(), TokenKind::QuoteMark),
                Token::new_with_parent(content.to_string(), TokenKind::Text, TokenKind::QuoteMark)
            ]
        )
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

        assert_eq!(ast.doc.len(), 1);
        assert_eq!(ast.doc[0].kind, Kind::DisorderedList);
        assert_eq!(
            ast.doc[0].sequence,
            vec![
                Token::new(mark.to_string(), TokenKind::DisorderListMark),
                Token::new_with_parent(
                    content.to_string(),
                    TokenKind::Text,
                    TokenKind::DisorderListMark
                )
            ]
        )
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

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc[0].kind, Kind::SortedList);
            assert_eq!(
                ast.doc[0].sequence,
                vec![
                    Token::new(mark.to_string(), TokenKind::SortedListMark),
                    Token::new_with_parent(
                        content.to_string(),
                        TokenKind::Text,
                        TokenKind::SortedListMark
                    )
                ]
            )
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

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc[0].kind, Kind::Blank);
            assert_eq!(
                ast.doc[0].sequence,
                vec![Token::new("".to_string(), TokenKind::BlankLine)]
            )
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

                assert_eq!(ast.doc.len(), 1);
                assert_eq!(ast.doc[0].kind, Kind::NormalText);
                if cnt.is_empty() {
                    // token 0
                    assert_eq!(ast.doc[0].sequence[0].kind, TokenKind::Picture);
                    assert_eq!(ast.doc[0].sequence[0].value, pic.to_string());
                } else {
                    // token 0
                    assert_eq!(ast.doc[0].sequence[0].kind, TokenKind::Text);
                    assert_eq!(ast.doc[0].sequence[0].value, cnt.to_string());

                    // token 1
                    assert_eq!(ast.doc[0].sequence[1].kind, TokenKind::Picture);
                    assert_eq!(ast.doc[0].sequence[1].value, pic.to_string());
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

                assert_eq!(ast.doc.len(), 1);
                assert_eq!(ast.doc[0].kind, Kind::NormalText);
                if cnt.is_empty() {
                    // token 0
                    assert_eq!(ast.doc[0].sequence[0].kind, TokenKind::Url);
                    assert_eq!(ast.doc[0].sequence[0].value, url.to_string());
                } else {
                    // token 0
                    assert_eq!(ast.doc[0].sequence[0].kind, TokenKind::Text);
                    assert_eq!(ast.doc[0].sequence[0].value, cnt.to_string());

                    // token 1
                    assert_eq!(ast.doc[0].sequence[1].kind, TokenKind::Url);
                    assert_eq!(ast.doc[0].sequence[1].value, url.to_string());
                }
            }
        }
    }

    #[test]
    fn test_code_mark() {
        let md = "``` ";

        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        assert_eq!(ast.doc.len(), md.lines().count());
        assert_eq!(ast.doc[0].kind, Kind::CodeMark);
        assert_eq!(
            ast.doc[0].sequence,
            vec![Token::new(md.trim_end().to_string(), TokenKind::CodeMark)]
        )
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

        assert_eq!(ast.doc.len(), md.lines().count());
        assert_eq!(ast.blocks.len(), 8);

        assert_eq!(ast.blocks[2].kind, Kind::Code);
        assert_eq!(ast.blocks[2].line_pointers.len(), 2);

        assert_eq!(ast.blocks[7].kind, Kind::NormalText);
        assert_eq!(ast.blocks[7].line_pointers.len(), 1);
    }
}
