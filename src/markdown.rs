use std::fmt;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
pub struct Ast<'ast> {
    doc: Vec<Line>,
    blocks: Vec<Block<'ast>>,
}

// Block is a group of multiple lines.
struct Block<'blk> {
    lines: Vec<&'blk Line>,
}

impl<'ast> Ast<'ast> {
    pub fn new() -> Self {
        Ast {
            doc: Vec::new(),
            blocks: Vec::new(),
        }
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
        self.doc.push(Line::new(num, line));
        // TODO:
        if let Some(_) = self.doc.last() {
            //   self.blocks.push(Block { lines: vec![l] })
        }
    }
}

impl<'ast> Default for Ast<'ast> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ast> Debug for Ast<'ast> {
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
    kind: LineKind,
    num: i32,
    text: String,
}

#[derive(Debug, PartialEq)]
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

#[derive(PartialEq, Debug)]
enum TokenKind {
    TitleMark,
    DisorderListMark,
    SortedListMark,
    DividingMark,
    QuoteMark,
    BoldMark,
    BlankLine,
    Text,
    LineBreak,
}

impl Line {
    fn new(ln: i32, line: String) -> Self {
        let mut l = Line {
            num: ln,
            text: line,
            kind: LineKind::Unknow,
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
            None => LineKind::Unknow,
            Some(t) => match t.kind {
                TokenKind::BlankLine => LineKind::Blank,
                TokenKind::TitleMark => LineKind::Title,
                TokenKind::DisorderListMark => LineKind::DisorderedList,
                TokenKind::SortedListMark => LineKind::SortedList,
                TokenKind::DividingMark => LineKind::DividingLine,
                TokenKind::QuoteMark => LineKind::Quote,
                _ => LineKind::Plain,
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
    // begin_in_line: usize,
    // end_in_line: usize,
}

impl Token {
    fn new(value: String, kind: TokenKind) -> Self {
        Token {
            value,
            kind,
            parent_kind: None,
        }
    }
    fn new_with_parent(value: String, kind: TokenKind, parent: TokenKind) -> Self {
        Token {
            value,
            kind,
            parent_kind: Some(parent),
        }
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
    Plain,
    Finished,
}

#[derive(PartialEq, Clone, Copy)]
enum SplitPlainState {
    Default,
    Bold(usize), // usize is start position in line
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
                if let Some(t) = self.split_list_item(cur_pos, cur_char) {
                    self.line_tokens.push(t)
                }
            }
            State::SortedList => {
                if let Some(t) = self.split_list_item(cur_pos, cur_char) {
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
            State::Plain => {
                if let Some(ts) = self.split_plain_text() {
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

    // skip all whitespace characters at the beginning of the line,
    // or generate a blank line token if the line contains only whitespace characters.
    fn skip_begin_whitespaces(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        if cur_char.is_whitespace() {
            if cur_char == '\n' {
                self.state = State::Finished;
                self.unparsed = cur_pos + 1;
                return Some(Token::new("".to_string(), TokenKind::BlankLine));
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
    fn split_title(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::new(rest.trim_end().to_string(), TokenKind::Text))
    }

    // parse the rest of the line as a list item token.
    fn split_list_item(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::new(rest.trim_end().to_string(), TokenKind::Text))
    }

    // parse the rest of the line as the quote token.
    fn split_quote(&mut self, cur_pos: usize, cur_char: char) -> Option<Token> {
        // skip all whitespace characters after the mark token.
        if cur_char.is_whitespace() {
            self.unparsed = cur_pos + 1;
            return None;
        }
        let rest = utf8_slice::from(self.line_text, cur_pos);
        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;
        Some(Token::new(rest.trim_end().to_string(), TokenKind::Text))
    }

    // parse the line as the plain token.
    fn split_plain_text(&mut self) -> Option<Vec<Token>> {
        let mut last = self.unparsed;
        let content = utf8_slice::from(self.line_text, last);

        self.unparsed = utf8_slice::len(self.line_text);
        self.state = State::Finished;

        let mut buff: Vec<Token> = Vec::new();
        let mut ss = SplitPlainState::Default;

        for (i, ch) in content.chars().enumerate() {
            if ch == '\n' {
                // end of the line
                buff.push(Token::new(
                    utf8_slice::slice(content, last, i)
                        .trim_end()
                        .trim_end_matches("<br>")
                        .to_string(),
                    TokenKind::Text,
                ));
                break;
            }
            match ss {
                SplitPlainState::Default => {
                    if ch == '*' {
                        ss = SplitPlainState::Bold(i);
                    }
                }
                SplitPlainState::Bold(begin) => {
                    if ch == '*' {
                        // so it's '**'.

                        // the part of the plain text before '**' mark.
                        let s = utf8_slice::slice(content, last, begin);
                        if !s.is_empty() {
                            buff.push(Token::new(s.to_string(), TokenKind::Text));
                        }
                        // '**' mark.
                        buff.push(Token::new("**".to_string(), TokenKind::BoldMark));

                        last = i + 1;
                    }
                    ss = SplitPlainState::Default;
                }
            }
        }

        if Self::has_line_break(content) {
            buff.push(Token::new("<br>".to_string(), TokenKind::LineBreak));
        }

        Some(buff)
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

> Rust, A language empowering everyone to build reliable and efficient software.";

        let mut ast = Ast::new();
        for (num, line) in md.lines().enumerate() {
            let mut s = line.to_string();
            s.push('\n');
            ast.push(num as i32, s);
        }
        assert_eq!(ast.doc.len(), md.lines().count());
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
                assert_eq!(l.kind, LineKind::Title);
                assert_eq!(
                    l.sequence,
                    vec![
                        Token::new(mark.to_string(), TokenKind::TitleMark),
                        Token::new(titles.get(0).unwrap().trim().to_string(), TokenKind::Text)
                    ]
                );
            }
            {
                let l = ast.doc.get(1).unwrap();
                assert_eq!(l.kind, LineKind::Title);
                assert_eq!(
                    l.sequence,
                    vec![
                        Token::new(mark.to_string(), TokenKind::TitleMark,),
                        Token::new(titles.get(1).unwrap().trim().to_string(), TokenKind::Text)
                    ]
                );
            }
            {
                let l = ast.doc.get(2).unwrap();
                assert_eq!(l.kind, LineKind::Title);
                assert_eq!(
                    l.sequence,
                    vec![
                        Token::new(mark.to_string(), TokenKind::TitleMark,),
                        Token::new(titles.get(2).unwrap().trim().to_string(), TokenKind::Text)
                    ]
                );
            }
            {
                let l = ast.doc.get(3).unwrap();
                assert_eq!(l.kind, LineKind::Title);
                assert_eq!(
                    l.sequence,
                    vec![Token::new(mark.to_string(), TokenKind::TitleMark,)]
                );
            }
            {
                let l = ast.doc.get(4).unwrap();
                assert_eq!(l.kind, LineKind::Title);
                assert_eq!(
                    l.sequence,
                    vec![Token::new(mark.to_string(), TokenKind::TitleMark,)]
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
            assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::DividingLine);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![Token::new(mark.trim().to_string(), TokenKind::DividingMark,)]
            )
        }
    }

    #[test]
    fn test_parse_plain() {
        let contents = vec![
            "这是我的一个学习 rust 编程语言的项目，我将尝试去开发一个强大的 markdown 编辑器。",
            "--- x",
            // "***xxxx",
            "___ 这不是一个分界线",
            "--- ---",
            // "*** ***",
            "___ ___",
            "#这不是标题",
            "##这也不是标题",
            ">这不是引用",
            "*这不是列表",
            "1.这也不是列表",
        ];

        for cnt in contents {
            let mut ast = Ast::new();
            let mut s = cnt.to_string();
            s += "\n";
            ast.push(1, s);

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::Plain);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![Token::new(cnt.trim().to_string(), TokenKind::Text)]
            )
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
            let mut ast = Ast::new();
            let mut s = cnt.to_string();
            s += "\n";
            ast.push(1, s);

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::Plain);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![
                    Token::new(
                        cnt.trim().trim_end_matches("<br>").to_string(),
                        TokenKind::Text,
                    ),
                    Token::new("<br>".to_string(), TokenKind::LineBreak,)
                ]
            )
        }
    }

    #[test]
    fn test_parse_quote() {
        let mut ast = Ast::new();
        let mark = ">";
        let content =
            "Rust, A language empowering everyone to build reliable and efficient software.";

        let mut s = mark.to_string();
        s.push_str(" ");
        s.push_str(content);
        ast.push(1, s);

        assert_eq!(ast.doc.len(), 1);
        assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::Quote);
        assert_eq!(
            ast.doc.get(0).unwrap().sequence,
            vec![
                Token::new(mark.to_string(), TokenKind::QuoteMark),
                Token::new(content.to_string(), TokenKind::Text)
            ]
        )
    }

    #[test]
    fn test_parse_disorder_list() {
        let mut ast = Ast::new();
        let mark = "*";
        let content =
            "Rust, A language empowering everyone to build reliable and efficient software.";

        let mut s = mark.to_string();
        s.push_str(" ");
        s.push_str(content);
        ast.push(1, s);

        assert_eq!(ast.doc.len(), 1);
        assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::DisorderedList);
        assert_eq!(
            ast.doc.get(0).unwrap().sequence,
            vec![
                Token::new(mark.to_string(), TokenKind::DisorderListMark,),
                Token::new(content.to_string(), TokenKind::Text)
            ]
        )
    }

    #[test]
    fn test_parse_sorted_list() {
        let marks = vec!["1.", "2.", "3.", "4.", "10.", "11.", "99.", "100.", "999."];
        let content =
            "Rust, A language empowering everyone to build reliable and efficient software.";

        for mark in marks {
            let mut ast = Ast::new();
            let mut s = mark.to_string();
            s.push_str(" ");
            s.push_str(content);
            ast.push(1, s);

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::SortedList);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![
                    Token::new(mark.to_string(), TokenKind::SortedListMark),
                    Token::new(content.to_string(), TokenKind::Text)
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
            let mut ast = Ast::new();
            let mut s = cnt.to_string();
            s += "\n";
            ast.push(1, s);

            assert_eq!(ast.doc.len(), 1);
            assert_eq!(ast.doc.get(0).unwrap().kind, LineKind::Blank);
            assert_eq!(
                ast.doc.get(0).unwrap().sequence,
                vec![Token::new("".to_string(), TokenKind::BlankLine,)]
            )
        }
    }
}
