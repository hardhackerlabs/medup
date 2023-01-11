use std::collections::{HashMap, VecDeque};

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
                        if p1.is_some() {
                            let mut t = Token::new(s.to_string(), TokenKind::Image);
                            let rf = &mut t;
                            rf.as_img_mut()
                                .insert_alt_name(utf8_slice::slice(content, p2 + 1, p3));
                            rf.as_img_mut()
                                .insert_location(utf8_slice::slice(content, p4 + 1, i));
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
                                .insert_location(utf8_slice::slice(content, p4 + 1, i));
                            buff.push(t);
                        };

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
        Self::tidy(&mut buff);
        buff
    }

    // TODO: need to optimize
    fn tidy(buff: &mut Vec<Token>) {
        let mut splits_at: VecDeque<(usize, usize)> = VecDeque::new();
        let mut pre = 0;
        for (ix, t) in buff
            .iter()
            .enumerate()
            .filter(|(_, t)| t.kind() == TokenKind::StarMark)
        {
            let n = t.value().len() - pre;
            if pre > 0 && n > 0 {
                let l = splits_at.len();
                splits_at.push_back((ix + l, pre));
                pre = n;
            } else {
                pre = t.value().len();
            }
        }
        splits_at.into_iter().for_each(|(ix, l)| {
            let off = buff[ix].split_off(l);
            buff.insert(ix + 1, off);
        });

        let mut buff_iter = buff
            .iter_mut()
            .filter(|t| t.kind() == TokenKind::StarMark)
            .peekable();
        while let Some(t) = buff_iter.next() {
            match t.value() {
                "*" | "**" | "***" => {
                    if buff_iter.peek().map(|n| n.value()).unwrap_or("") == t.value() {
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
                    } else {
                        t.kind = TokenKind::Text;
                    }
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

    fn split_off(&mut self, at: usize) -> Token {
        let off = self.value.split_off(at);
        Token::new(off, self.kind())
    }

    fn insert(&mut self, k: &str, v: &str) {
        self.details
            .get_or_insert(HashMap::new())
            .insert(k.to_string(), v.to_string());
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
