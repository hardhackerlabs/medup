use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
};

use itertools::Itertools;

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
    Continuous(usize),
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
pub(crate) struct Lexer<'lex> {
    state: State,
    line_text: &'lex str,
}

impl<'lex> Lexer<'lex> {
    pub(crate) fn new(text: &'lex str) -> Self {
        Lexer {
            state: State::Begin,
            line_text: text,
        }
    }

    pub(crate) fn split(mut self) -> Vec<Token> {
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
            for t in Lexer::split_inside(rest)
                .into_iter()
                .filter(|t| !t.value().is_empty())
            {
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
        while let Some((ix, ch)) = content_iter.next() {
            match (state, ch) {
                (_, '\n') => {
                    // end of the line
                    let s = utf8_slice::slice(content, last, ix)
                        .trim_end()
                        .trim_end_matches("<br>")
                        .to_string();
                    if !s.is_empty() {
                        buff.push(Token::new(s, TokenKind::Text));
                    }
                    break;
                }
                (TextState::Normal, _) => match ch {
                    '*' | '_' => {
                        // the part of normal text before '*' mark.
                        let s = utf8_slice::slice(content, last, ix);
                        if !s.is_empty() {
                            buff.push(Token::new(s.to_string(), TokenKind::Text));
                        }

                        last = ix;

                        if *content_iter.peek().map(|(_, n)| n).unwrap_or(&' ') == ch {
                            state = TextState::Continuous(ix);
                        } else {
                            // '*' mark
                            let s = utf8_slice::slice(content, ix, ix + 1);
                            last = ix + 1;
                            let k = match ch {
                                '*' => TokenKind::StarMark,
                                '_' => TokenKind::UnderLineMark,
                                _ => unreachable!(),
                            };
                            buff.push(Token::new(s.to_string(), k));
                        }
                    }
                    '!' => state = TextState::PicBegin(ix), // begin of picture
                    '[' => state = TextState::UrlTitleBegin(ix), // begin of url
                    _ => (),
                },
                (TextState::PicBegin(p), _) => match ch {
                    '[' => state = TextState::PicTitleBegin(p, ix),
                    '!' => state = TextState::PicBegin(ix),
                    _ => state = TextState::Normal,
                },
                (TextState::PicTitleBegin(p1, p2), _) => {
                    if ch == ']' {
                        state = TextState::TitleDone(Some(p1), p2, ix);
                    }
                }
                (TextState::UrlTitleBegin(p), _) => match ch {
                    ']' => state = TextState::TitleDone(None, p, ix),
                    '[' => state = TextState::UrlTitleBegin(ix),
                    _ => (),
                },
                (TextState::TitleDone(p1, p2, p3), _) => match ch {
                    '(' => state = TextState::LocationBegin(p1, p2, p3, ix),
                    ']' => state = TextState::TitleDone(p1, p2, ix),
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
                        let s = utf8_slice::slice(content, begin, ix + 1);
                        if p1.is_some() {
                            let mut t = Token::new(s.to_string(), TokenKind::Image);
                            let rf = &mut t;
                            rf.as_img_mut()
                                .insert_alt_name(utf8_slice::slice(content, p2 + 1, p3));
                            rf.as_img_mut()
                                .insert_location(utf8_slice::slice(content, p4 + 1, ix));
                            buff.push(t);
                        } else {
                            let mut t = Token::new(s.to_string(), TokenKind::Url);
                            let rf = &mut t;
                            rf.as_url_mut().insert_show_name(utf8_slice::slice(
                                content,
                                p2 + 1,
                                p3,
                            ));
                            rf.as_url_mut()
                                .insert_location(utf8_slice::slice(content, p4 + 1, ix));
                            buff.push(t);
                        };

                        last = ix + 1;
                        state = TextState::Normal;
                    }
                }
                (TextState::Continuous(begin), _) => {
                    if *content_iter.peek().map(|(_, n)| n).unwrap_or(&' ') != ch {
                        let s = utf8_slice::slice(content, begin, ix + 1);
                        let k = match ch {
                            '*' => TokenKind::StarMark,
                            '_' => TokenKind::UnderLineMark,
                            _ => unreachable!(),
                        };
                        buff.push(Token::new(s.to_string(), k));

                        last = ix + 1;
                        state = TextState::Normal;
                    }
                }
            }
        }
        if Lexer::has_br(content) {
            buff.push(Token::new("<br>".to_string(), TokenKind::LineBreak));
        }
        Lexer::tidy(&mut buff);
        buff
    }

    fn tidy(buff: &mut Vec<Token>) {
        Lexer::tidy_continuous_mark(TokenKind::StarMark, buff);
        Lexer::tidy_continuous_mark(TokenKind::UnderLineMark, buff);

        let mut stack: Vec<&mut Token> = vec![];

        let buff_iter = buff
            .iter_mut()
            .filter(|t| t.kind() == TokenKind::StarMark || t.kind() == TokenKind::UnderLineMark);

        for t in buff_iter {
            if let Some(revp) = stack
                .iter()
                .rev()
                .find_position(|e| e.kind() == t.kind() && e.value() == t.value())
                .map(|(x, _)| x)
            {
                // Notice: the 'revp' is a reversed positional index on the stack
                let p = stack.len() - 1 - revp;

                // found in stack
                loop {
                    if let Some(pop) = stack.pop() {
                        if stack.len() == p {
                            match t.value() {
                                "*" | "_" => {
                                    pop.update_kind(TokenKind::ItalicMark);
                                    t.update_kind(TokenKind::ItalicMark);
                                }
                                "**" | "__" => {
                                    pop.update_kind(TokenKind::BoldMark);
                                    t.update_kind(TokenKind::BoldMark);
                                }
                                "***" | "___" => {
                                    pop.update_kind(TokenKind::ItalicBoldMark);
                                    t.update_kind(TokenKind::ItalicBoldMark);
                                }
                                _ => unreachable!(),
                            }
                            break;
                        } else {
                            pop.update_kind(TokenKind::Text);
                        }
                    }
                }
            } else {
                // not found in stack
                if t.len() < 4 {
                    stack.push(t);
                } else {
                    t.update_kind(TokenKind::Text)
                }
            }
        }

        // At last, we need to update the elment left on the stack
        stack
            .iter_mut()
            .for_each(|e| e.update_kind(TokenKind::Text));
    }

    fn tidy_continuous_mark(kind: TokenKind, buff: &mut Vec<Token>) {
        let mut splits_at: VecDeque<(usize, usize)> = VecDeque::new();

        let mut pre: usize = 0;

        for (ix, t) in buff.iter().enumerate().filter(|(_, t)| t.kind() == kind) {
            let n = if t.len() >= pre && pre > 0 {
                t.len() - pre
            } else {
                pre = t.len();
                continue;
            };
            match n.cmp(&0) {
                // need to split current token
                Ordering::Greater => {
                    let l = splits_at.len();
                    splits_at.push_back((ix + l, pre));

                    pre = n;
                }
                // matched with previous token
                Ordering::Equal => {
                    pre = 0;
                }
                Ordering::Less => unreachable!(),
            }
        }
        splits_at.into_iter().for_each(|(ix, l)| {
            let off = buff[ix].split_off(l);
            buff.insert(ix + 1, off);
        });
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum TokenKind {
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
    UnderLineMark,
    WhiteSpace,
}

// Token is a part of the line, the parser will parse the line into some tokens.
#[derive(PartialEq, Debug)]
pub(crate) struct Token {
    value: String,
    kind: TokenKind,
    pub(crate) details: Option<HashMap<String, String>>,
}

impl Token {
    pub(crate) fn new(value: String, kind: TokenKind) -> Self {
        Token {
            value,
            kind,
            details: None,
        }
    }

    // Get value of the token
    pub(crate) fn value(&self) -> &str {
        &self.value
    }

    // Get the value length of the token
    pub(crate) fn len(&self) -> usize {
        self.value.len()
    }

    // Get kind of the token
    pub(crate) fn kind(&self) -> TokenKind {
        self.kind
    }

    // Set a new kind of the token
    pub(crate) fn update_kind(&mut self, kind: TokenKind) {
        self.kind = kind
    }

    // convert the token to url token
    pub(crate) fn as_url(&self) -> UrlToken {
        if self.kind() != TokenKind::Url {
            panic!("token is not url");
        }
        UrlToken(self)
    }
    fn as_url_mut(&mut self) -> UrlTokenAsMut {
        if self.kind() != TokenKind::Url {
            panic!("token is not url");
        }
        UrlTokenAsMut(self)
    }

    // convert the token to img token
    pub(crate) fn as_img(&self) -> ImgToken {
        if self.kind() != TokenKind::Image {
            panic!("token is not image");
        }
        ImgToken(self)
    }
    fn as_img_mut(&mut self) -> ImgTokenAsMut {
        if self.kind() != TokenKind::Image {
            panic!("token is not image");
        }
        ImgTokenAsMut(self)
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
}

#[derive(PartialEq, Debug)]
pub(crate) struct UrlToken<'url_token>(&'url_token Token);

impl<'url_token> UrlToken<'url_token> {
    // Get show name of the URL
    pub(crate) fn get_show_name(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("show_name").map(|x| &**x))
    }

    // Get location of the URL
    pub(crate) fn get_location(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("location").map(|x| &**x))
    }
}

#[derive(PartialEq, Debug)]
pub(crate) struct UrlTokenAsMut<'url_token>(&'url_token mut Token);

impl<'url_token> UrlTokenAsMut<'url_token> {
    fn insert_show_name(&mut self, v: &str) {
        self.0.insert("show_name", v)
    }

    fn insert_location(&mut self, v: &str) {
        self.0.insert("location", v)
    }
}

#[derive(PartialEq, Debug)]
pub(crate) struct ImgToken<'img_token>(&'img_token Token);

impl<'img_token> ImgToken<'img_token> {
    // Get alt name of the Image
    pub(crate) fn get_alt_name(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("alt_name").map(|x| &**x))
    }

    // Get location of the Image
    pub(crate) fn get_location(&self) -> Option<&str> {
        self.0
            .details
            .as_ref()
            .and_then(|x| x.get("location").map(|x| &**x))
    }
}

#[derive(PartialEq, Debug)]
pub(crate) struct ImgTokenAsMut<'img_token>(&'img_token mut Token);

impl<'img_token> ImgTokenAsMut<'img_token> {
    fn insert_alt_name(&mut self, v: &str) {
        self.0.insert("alt_name", v)
    }

    fn insert_location(&mut self, v: &str) {
        self.0.insert("location", v)
    }
}

mod tests {
    use super::*;

    fn exec_cases(cases: Vec<(&str, Vec<(&str, TokenKind)>)>) {
        for c in cases.iter() {
            let s = if c.0.ends_with("\n") {
                c.0.to_string()
            } else {
                let mut s1 = c.0.to_string();
                s1.push('\n');
                s1
            };

            assert_eq!(
                Lexer::new(s.as_str()).split(),
                c.1.iter()
                    .map(|(v, k)| { Token::new(v.to_string(), *k) })
                    .collect::<Vec<Token>>()
            );
        }
    }

    #[test]
    fn test_normal_text() {
        let cases = vec![
            ("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。",
                vec![("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。", TokenKind::Text)],
            ),
            ("--- x", vec![("--- x", TokenKind::Text)]),
            (
                "___ 这不是一个分界线",
                vec![
                    ("___", TokenKind::Text),
                    (" 这不是一个分界线", TokenKind::Text),
                ],
            ),
            ("#这不是标题", vec![("#这不是标题", TokenKind::Text)]),
            ("##这也不是标题", vec![("##这也不是标题", TokenKind::Text)]),
            (">这不是引用", vec![(">这不是引用", TokenKind::Text)]),
            ("1.这也不是列表", vec![("1.这也不是列表", TokenKind::Text)]),
            (
                "***xxxx",
                vec![("***", TokenKind::Text), ("xxxx", TokenKind::Text)],
            ),
        ];

        exec_cases(cases);
    }

    #[test]
    fn test_bold_italic() {
        let cases = vec![
            (
                "**粗体**_斜体_***斜体+粗体***",
                vec![
                    ("**", TokenKind::BoldMark),
                    ("粗体", TokenKind::Text),
                    ("**", TokenKind::BoldMark),
                    ("_", TokenKind::ItalicMark),
                    ("斜体", TokenKind::Text),
                    ("_", TokenKind::ItalicMark),
                    ("***", TokenKind::ItalicBoldMark),
                    ("斜体+粗体", TokenKind::Text),
                    ("***", TokenKind::ItalicBoldMark),
                ],
            ),
            (
                "**1** ****2***",
                vec![
                    ("**", TokenKind::BoldMark),
                    ("1", TokenKind::Text),
                    ("**", TokenKind::BoldMark),
                    (" ", TokenKind::Text),
                    ("****", TokenKind::Text),
                    ("2", TokenKind::Text),
                    ("***", TokenKind::Text),
                ],
            ),
            (
                "**__2__**",
                vec![
                    ("**", TokenKind::BoldMark),
                    ("__", TokenKind::BoldMark),
                    ("2", TokenKind::Text),
                    ("__", TokenKind::BoldMark),
                    ("**", TokenKind::BoldMark),
                ],
            ),
            (
                "**1**",
                vec![
                    ("**", TokenKind::BoldMark),
                    ("1", TokenKind::Text),
                    ("**", TokenKind::BoldMark),
                ],
            ),
            (
                "*1*",
                vec![
                    ("*", TokenKind::ItalicMark),
                    ("1", TokenKind::Text),
                    ("*", TokenKind::ItalicMark),
                ],
            ),
            (
                "*** 1 ***",
                vec![
                    ("***", TokenKind::ItalicBoldMark),
                    (" 1 ", TokenKind::Text),
                    ("***", TokenKind::ItalicBoldMark),
                ],
            ),
            (
                "__1__",
                vec![
                    ("__", TokenKind::BoldMark),
                    ("1", TokenKind::Text),
                    ("__", TokenKind::BoldMark),
                ],
            ),
            (
                "_1_",
                vec![
                    ("_", TokenKind::ItalicMark),
                    ("1", TokenKind::Text),
                    ("_", TokenKind::ItalicMark),
                ],
            ),
            (
                "___ 1 ___",
                vec![
                    ("___", TokenKind::ItalicBoldMark),
                    (" 1 ", TokenKind::Text),
                    ("___", TokenKind::ItalicBoldMark),
                ],
            ),
        ];

        exec_cases(cases);
    }

    #[test]
    fn test_line_break() {
        let cases = vec![
            ("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。  ",  // have two spaces at the end of the line.
                vec![("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。", TokenKind::Text),
                    ("<br>", TokenKind::LineBreak)],
            ),
            ("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。       ", // have two tab spaces at the end of the line.
                vec![("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。", TokenKind::Text),
                    ("<br>", TokenKind::LineBreak)],
            ),
            ("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。    <br>  ", // have two spaces at the end of the line.
                vec![("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。    ", TokenKind::Text),
                    ("<br>", TokenKind::LineBreak)],
            ),
            ("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。<br>",
                vec![("这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。", TokenKind::Text),
                    ("<br>", TokenKind::LineBreak)],
            ),
        ];

        exec_cases(cases);
    }

    #[test]
    fn test_blank_line() {
        let cases = vec![
            ("\n", vec![("", TokenKind::BlankLine)]),
            (" \n", vec![(" ", TokenKind::BlankLine)]),
            ("     \n", vec![("     ", TokenKind::BlankLine)]),
            ("         ", vec![("         ", TokenKind::BlankLine)]),
            (
                "                                            ",
                vec![(
                    "                                            ",
                    TokenKind::BlankLine,
                )],
            ),
            ("  ", vec![("  ", TokenKind::BlankLine)]),
        ];

        exec_cases(cases);
    }

    #[test]
    fn test_dividing() {
        let cases = vec![
            ("---", vec![("---", TokenKind::DividingMark)]),
            ("***", vec![("***", TokenKind::DividingMark)]),
            ("___", vec![("___", TokenKind::DividingMark)]),
            ("------", vec![("------", TokenKind::DividingMark)]),
            ("****", vec![("****", TokenKind::DividingMark)]),
            (
                "__________         ",
                vec![("__________", TokenKind::DividingMark)],
            ),
            (
                "----------------------------------------   ",
                vec![(
                    "----------------------------------------",
                    TokenKind::DividingMark,
                )],
            ),
        ];

        exec_cases(cases);
    }
}
