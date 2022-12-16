use std::fmt;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
pub struct Ast {
    doc: Vec<Line>,
    blocks: Vec<Block>,
}

// Block is a group of multiple lines.
struct Block {
    line_pointers: Vec<usize>, // it stores the index of 'Line' struct in 'ast.doc'
    kind: Kind,
}

impl Ast {
    pub fn new() -> Self {
        Ast {
            doc: Vec::new(),
            blocks: Vec::new(),
        }
    }

    pub fn parse_reader(&mut self, mut reader: BufReader<File>) {
        let mut num: usize = 0;
        loop {
            let mut buf = String::new();
            let num_bytes = reader.read_line(&mut buf).unwrap();
            if num_bytes == 0 {
                break;
            }
            num += 1;
            self.push(num, buf);
        }

        self.build_blocks();
    }

    pub fn parse_file(&mut self, path: &str) {
        let file = File::open(path).unwrap();
        let reader = BufReader::new(file);
        self.parse_reader(reader);
    }
    pub fn parse_string(&mut self, s: &str) {}

    fn push(&mut self, num: usize, line: String) {
        self.doc.push(Line::new(num, line));
    }

    fn build_blocks(&mut self) {
        for (i, l) in self.doc.iter().enumerate() {
            match l.kind {
                Kind::Blank | Kind::DisorderedList | Kind::SortedList => {
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

// Line is a line of the markdown file, it be parsed into some tokens.
struct Line {
    sequence: Vec<Token>,
    kind: Kind,
    num: usize,
    text: String,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Kind {
    Unknow,
    Blank,
    Title,
    DisorderedList,
    SortedList,
    DividingLine,
    Quote,
    NormalText,
}

#[derive(PartialEq, Debug)]
enum TokenKind {
    TitleMark,        // #, ##, ###, ####
    DisorderListMark, // *
    SortedListMark,   // 1.
    DividingMark,     // ---, ***, ___
    QuoteMark,        // >
    BoldMark,         // ** **
    BlankLine,        // \n
    Text,             //
    LineBreak,        // <br>, double whitespace
    Picture,          // ![]()
    Url,              // []()
}

impl Line {
    fn new(ln: usize, line: String) -> Self {
        let mut l = Line {
            num: ln,
            text: line,
            kind: Kind::Unknow,
            sequence: Vec::new(),
        };
        l.parse();
        l
    }
    // parses a line of text into 'Line' struct that contains multi tokens.
    fn parse(&mut self) {
        let mut lx = Lexer::new(&self.text, &mut self.sequence);

        for (cur_pos, cur_char) in self.text.chars().enumerate() {
            let stopped = lx.process(cur_pos, cur_char);
            if stopped {
                break;
            }
        }
        lx.finish();

        self.kind = match self.sequence.first() {
            None => Kind::Unknow,
            Some(t) => match t.kind {
                TokenKind::BlankLine => Kind::Blank,
                TokenKind::TitleMark => Kind::Title,
                TokenKind::DisorderListMark => Kind::DisorderedList,
                TokenKind::SortedListMark => Kind::SortedList,
                TokenKind::DividingMark => Kind::DividingLine,
                TokenKind::QuoteMark => Kind::Quote,
                _ => Kind::NormalText,
            },
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
    CheckMark,
    Title,
    DisorderedList,
    SortedList,
    Quote,
    CheckDividing,
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
                if let Some(t) = self.skip_begin_whitespaces(cur_pos, cur_char) {
                    self.line_tokens.push(t)
                }
            }
            State::CheckMark => {
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
            State::CheckDividing => {
                if let Some(t) = self.check_dividing(cur_pos, cur_char) {
                    self.line_tokens.push(t)
                }
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
    fn skip_begin_whitespaces(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            if cur_char == '\n' {
                self.end();
                return Some(Token::new("".to_string(), TokenKind::BlankLine));
            }
        } else {
            self.set_state(cur_pos, State::CheckMark);
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
                    State::CheckDividing,
                    Some(Token::new(first_word.to_string(), TokenKind::DividingMark)),
                )
            }

            // quote
            ['>'] => (
                cur_pos + 1,
                State::Quote,
                Some(Token::new(first_word.to_string(), TokenKind::QuoteMark)),
            ),

            // normal (as no mark)
            _ => {
                // don't change the unparsed pointer, because the first word is not a mark.
                (0, State::Normal, None)
            }
        };

        self.set_state(unparsed, state);
        token
    }

    // if there is not a valid dividing line, will recreate the token as normal.
    fn check_dividing(&mut self, _cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            return None;
        }
        // if contains whitespace character, it's a invalid dividing line
        self.line_tokens.clear(); // not a valid dividing line, so clear the dividing mark token
        self.set_state(0, State::Normal);
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
        let mut buff = Self::parse_inside(self.line_text);
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

#[cfg(test)]
mod tests {
    use super::*;

    fn create_ast(s: &str) -> Ast {
        let mut ast = Ast::new();
        let mut s = s.to_string();
        s += "\n";
        ast.push(1, s);
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
        for (num, line) in md.lines().enumerate() {
            let mut s = line.to_string();
            s.push('\n');
            ast.push(num, s);
        }
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
        for (num, line) in md.lines().enumerate() {
            let mut s = line.to_string();
            s.push('\n');
            ast.push(num, s);
        }
        ast.build_blocks();

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
                ast.push(n, s)
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
            " ------",
            "****",
            "  __________         ",
            " ----------------------------------------   ",
        ];

        for mark in marks {
            let mut ast = Ast::new();
            let mut s = mark.to_string();
            s += "\n";
            ast.push(1, s);

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, Kind::DividingLine);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![Token::new(mark.trim().to_string(), TokenKind::DividingMark,)]
            )
        }
    }

    #[test]
    fn test_parse_normal() {
        {
            let contents = vec![
                "   这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。",
                "--- x",
                "___ 这不是一个分界线",
                "--- ---",
                "___ ___",
                "       #这不是标题",
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
            "",
            " ",
            "     ",
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
}
