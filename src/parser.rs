use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::rc::Rc;
use std::{fmt, io};

use crate::lexer::{Lexer, Token, TokenKind};
use crate::Generate;
use crate::SharedLine;

use itertools::Itertools;
use v_htmlescape as htmlescape;

// Ast represents the abstract syntax tree of the markdown file, it structurally represents the entire file.
pub struct Ast {
    // Store all parsed line structs in order
    document: Vec<SharedLine>,
    // Related lines are compressed into the same block
    blocks: Vec<Block>,
    // Store all tags of the ref link, the map is "tag -> (location, title)"
    ref_link_tags: HashMap<String, (String, String)>,
    // The lines of the table of contents, it's a unordered list
    toc: Vec<SharedLine>,
}

impl Ast {
    // Create a Ast instance.
    pub(crate) fn new() -> Self {
        Ast {
            document: vec![Rc::new(RefCell::new(Line::meta()))],
            blocks: vec![],
            ref_link_tags: HashMap::new(),
            toc: vec![],
        }
    }

    // TODO:
    pub(crate) fn _insert_line(&mut self, _ln: usize, _s: &str) {}

    // TODO:
    pub(crate) fn _update_line(&mut self, _ln: usize, _s: &str) {}

    // TODO:
    pub(crate) fn _delete_line(&mut self, _ln: usize) {}

    // Parse markdown document from a file, the 'path' argument is the file path.
    pub(crate) fn parse_file(&mut self, path: &str) -> Result<(), io::Error> {
        let file = File::open(path)?;
        self.parse_from(&mut BufReader::new(file))
    }

    // Parse markdown document from a string.
    pub(crate) fn parse_string(&mut self, s: &str) -> Result<(), io::Error> {
        self.parse_from(&mut s.as_bytes())
    }

    // Parse markdown document from a reader, the 'reader' may be a file reader, byte buff or network socket etc.
    pub(crate) fn parse_from(&mut self, reader: &mut dyn BufRead) -> Result<(), io::Error> {
        let mut is_lazy = false;
        let mut lazy_queue: Vec<SharedLine> = vec![];

        let mut ln: usize = 0;

        loop {
            let (n, buf) = Self::read_line(reader)?;
            if n == 0 {
                break;
            }
            ln += 1;

            let l = Line::new(ln, buf);
            let lrc = Rc::new(RefCell::new(l));

            // add the scope block to reduce the lifecycle of the 'l' (l = lrc.borrow_mut())
            {
                let mut l = lrc.borrow_mut();

                match (l.weak_parse(), is_lazy) {
                    (Kind::CodeBlockMark, true) => {
                        // need to close the lazy parsing
                        is_lazy = false;
                        lazy_queue.clear();
                        // parse it really
                        l.parse();
                    }
                    (Kind::CodeBlockMark, false) => {
                        // need to open the lazy parsing
                        is_lazy = true;
                        // parse it really
                        l.parse();
                    }
                    (_, true) => {
                        // lazy parsing
                        lazy_queue.push(Rc::clone(&lrc));
                        l.without_parse(Kind::CodeBlock);
                    }
                    _ => {
                        // parse it really
                        l.parse();
                    }
                }

                // postpone
                l.pick_reflink_tags(&mut self.ref_link_tags);
            }
            self.document.push(lrc);

            debug_assert_eq!(self.count_lines(), ln);
            debug_assert_eq!(self.document[ln].borrow().num, ln);
        } // end of loop

        lazy_queue.iter().for_each(|l| l.borrow_mut().parse());
        lazy_queue.clear();
        self.blocks = Self::establish_blocks(&self.document);
        self.init_toc_lines();

        Ok(())
    }

    fn read_line(reader: &mut dyn BufRead) -> Result<(usize, String), io::Error> {
        let mut buf = String::new();
        let num_bytes = reader.read_line(&mut buf)?;
        if num_bytes == 0 {
            return Ok((0, "".to_string()));
        }
        if !buf.ends_with('\n') {
            buf.push('\n');
        }
        Ok((num_bytes, buf))
    }

    // Parse quote block into a new ast
    fn parse_quote_block(blocks: &mut [Block]) {
        for b in blocks.iter_mut().filter(|b| b.kind() == Kind::Quote) {
            let mut ast = Ast::new();

            // Since there is a newline(\n) character at the end of each line, so we use empty string ("") to join them
            let text = b
                .contains()
                .iter()
                .map(|e| {
                    let e = e.borrow();
                    let last = e.last_token();
                    if last.kind() == TokenKind::Text {
                        last.value().to_string()
                    } else {
                        "\n".to_string()
                    }
                })
                .collect::<Vec<String>>()
                .join("");

            ast.parse_string(&text).unwrap_or_else(|_e| unreachable!());
            b.quote_ast = Some(ast);
        }
    }

    // Generate the contents of the document
    pub(crate) fn generate_content(&self, generator: &impl Generate) -> String {
        self.generate(self.blocks(), generator)
    }

    // Generate the table of contents based on the title blocks and we skipped the level 1 title
    pub(crate) fn generate_toc(&self, generator: &impl Generate) -> String {
        let blocks = Self::establish_blocks(&self.toc);
        self.generate(&blocks, generator)
    }

    // Iterate through each block of the Ast and process the block into a 'html' string
    fn generate(&self, blocks: &[Block], generator: &impl Generate) -> String {
        blocks
            .iter()
            .filter(|b| b.kind() != Kind::Meta__ && b.kind() != Kind::ListNesting__)
            .map(|b| match b.kind() {
                Kind::Title => generator.render_title(b.first()),
                Kind::PlainText => generator.render_plain_text(b.contains()),
                Kind::Dividing => generator.render_dividing(b.first()),
                Kind::CodeBlock => generator.render_code(b.contains()),
                Kind::UnorderedList => generator.render_unordered_list(b.contains()),
                Kind::Blank => generator.render_blank(b.contains()),
                Kind::Quote => {
                    let s = b
                        .quote_ast
                        .as_ref()
                        .map(|a| a.generate_content(generator))
                        .unwrap_or_else(|| "".to_string());
                    generator.render_quote(&s)
                }
                Kind::OrderedList => generator.render_ordered_list(b.contains()),
                Kind::CodeBlockMark => generator.render_plain_text(b.contains()), // treat code block mark as plain text
                _ => unreachable!(),
            })
            .filter(|s| !s.is_empty())
            .join("\n\n")
    }

    // Count the lines in ast
    pub(crate) fn count_lines(&self) -> usize {
        self.document.len() - 1
    }

    pub(crate) fn ref_link_tags(&self) -> &HashMap<String, (String, String)> {
        &self.ref_link_tags
    }

    fn blocks(&self) -> &Vec<Block> {
        &self.blocks
    }

    fn init_toc_lines(&mut self) {
        const MIN_LEVEL: usize = 1;
        const MAX_LEVEL: usize = 6;

        for l in self.document.iter().filter(|l| {
            l.borrow().kind == Kind::Title
                && l.borrow().mark_token().len() >= MIN_LEVEL
                && l.borrow().mark_token().len() <= MAX_LEVEL
        }) {
            let mut buff: Vec<Token> = vec![];

            // create a new indent token with white space.
            let level = l.borrow().mark_token().len();
            if level > MIN_LEVEL {
                buff.push(Token::new(
                    "  ".repeat(level - MIN_LEVEL),
                    TokenKind::WhiteSpace,
                ));
            }

            // create a new unordered mark token
            buff.push(Token::new("*".to_string(), TokenKind::UnorderedMark));

            // create a new link token
            let (id, name) = l.borrow().anchor();
            let location = format!("#{}", id);

            let mut t = Token::new(format!("[{}]({})", name, location), TokenKind::Link);
            t.as_generic_link_mut().insert_name(&name);
            t.as_generic_link_mut().insert_location(&location);
            buff.push(t);

            // create a new line for toc
            let l2 = Line {
                kind: Kind::UnorderedList,
                buff,
                num: 0,
                text: name,
                nested_lines: vec![],
                nested_blocks: vec![],
            };

            self.toc.push(Rc::new(RefCell::new(l2)));
        }
    }

    fn establish_blocks(all: &[SharedLine]) -> Vec<Block> {
        let mut blocks: Vec<Block> = vec![];

        let mut leader: Option<&SharedLine> = None;
        let mut state: Option<Kind> = None;

        let mut iter = all
            .iter()
            .filter(|x| x.borrow().kind != Kind::Meta__)
            .peekable();
        while let Some(l) = iter.next() {
            let mut curr_line = l.borrow_mut();
            // Note: Don't use 'continue' statement in the match expression, because
            // we will save the kind of the previous block after the 'match'.
            match state.unwrap_or(curr_line.kind) {
                Kind::UnorderedList | Kind::OrderedList => {
                    if let Some(b) = blocks.last_mut().filter(|b| b.kind() == curr_line.kind) {
                        b.push(Rc::clone(l));
                    } else {
                        Self::insert_block(&mut blocks, Block::new(Rc::clone(l), curr_line.kind));
                    }

                    // determine whether the next line is a list nesting
                    if let Some(next) = iter.peek() {
                        if next.borrow().is_nested(&curr_line) > 0 {
                            state = Some(Kind::ListNesting__);
                            leader = Some(l); // save the previous line object as leader
                        }
                    }
                }
                Kind::ListNesting__ => {
                    // leader must not be None
                    debug_assert!(leader.is_some());

                    if let Some(ld) = leader {
                        let mut ld = ld.borrow_mut();
                        ld.nested_lines.push(Rc::clone(l));

                        if let Some(next) = iter.peek() {
                            if next.borrow().is_nested(&ld) <= 0 {
                                (state, leader) = (None, None);
                            }
                        }
                    }
                }
                Kind::CodeBlockMark => {
                    let mut k: Option<Kind> = None;
                    if let Some(next) = iter.peek() {
                        let next = next.borrow();
                        if next.kind == Kind::CodeBlockMark || next.kind == Kind::CodeBlock {
                            k = Some(Kind::CodeBlock);
                            state = Some(Kind::CodeBlock);
                        }
                    }
                    Self::insert_block(
                        &mut blocks,
                        Block::new(Rc::clone(l), k.unwrap_or(Kind::CodeBlockMark)),
                    );
                }
                Kind::CodeBlock => {
                    let b = blocks.last_mut().filter(|b| b.kind() == Kind::CodeBlock);
                    debug_assert!(b.is_some());

                    if let Some(b) = b {
                        b.push(Rc::clone(l));
                    }

                    if curr_line.kind == Kind::CodeBlockMark {
                        // close this code block
                        state = None;
                    }
                }
                Kind::Blank | Kind::Quote | Kind::PlainText => {
                    if let Some(b) = blocks.last_mut().filter(|b| b.kind() == curr_line.kind) {
                        b.push(Rc::clone(l));
                    } else {
                        Self::insert_block(&mut blocks, Block::new(Rc::clone(l), curr_line.kind));
                    }
                }
                Kind::Dividing => {
                    // get kind of the previous line
                    let prev = all.get(curr_line.num - 1).map(|v| v.borrow().kind);
                    // get kind of the next line
                    let next = iter.peek().map(|v| v.borrow().kind);

                    if prev.unwrap_or(Kind::Blank) == Kind::Blank
                        && next.unwrap_or(Kind::Blank) == Kind::Blank
                    {
                        Self::insert_block(&mut blocks, Block::new(Rc::clone(l), Kind::Dividing))
                    } else {
                        // convert dividing to plain text
                        curr_line.kind = Kind::PlainText;
                        curr_line
                            .buff
                            .iter_mut()
                            .for_each(|t| t.downgrade_to_text());

                        if let Some(b) = blocks.last_mut().filter(|b| b.kind() == Kind::PlainText) {
                            b.push(Rc::clone(l));
                        } else {
                            Self::insert_block(
                                &mut blocks,
                                Block::new(Rc::clone(l), Kind::PlainText),
                            );
                        }
                    }
                }
                Kind::Title => {
                    Self::insert_block(&mut blocks, Block::new(Rc::clone(l), Kind::Title));
                }
                Kind::Meta__ => unreachable!(),
            } // end of match
        } // end of while

        // build nested lists recursively
        for b in blocks
            .iter()
            .filter(|b| b.kind() == Kind::UnorderedList || b.kind() == Kind::OrderedList)
        {
            b.contains()
                .iter()
                .filter(|l| !l.borrow().nested_lines.is_empty())
                .for_each(|l| {
                    let mut l = l.borrow_mut();
                    let mut bs = Self::establish_blocks(&l.nested_lines);
                    l.nested_blocks.append(&mut bs);
                });
        }

        Self::parse_quote_block(&mut blocks);
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

#[derive(Debug, PartialEq, Copy, Clone)]
enum Kind {
    PlainText,
    Blank,
    Title,
    UnorderedList,
    OrderedList,
    Dividing,
    Quote,
    CodeBlockMark,
    CodeBlock,
    Meta__,
    ListNesting__,
}

// Block is a combination of associated lines.
#[derive(Debug)]
struct Block {
    seq: usize,
    kind: Kind,
    contains: Vec<SharedLine>,
    quote_ast: Option<Ast>,
}

impl Block {
    fn new(l: SharedLine, kind: Kind) -> Self {
        Block {
            contains: vec![l],
            kind,
            seq: 0,
            quote_ast: None,
        }
    }

    fn contains(&self) -> &Vec<SharedLine> {
        &self.contains
    }

    fn first(&self) -> &SharedLine {
        &self.contains[0]
    }

    fn kind(&self) -> Kind {
        self.kind
    }

    fn push(&mut self, l: SharedLine) {
        self.contains.push(l)
    }
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
    //   Kind::UnorderedList
    //   Kind::OrderedList
    //   Kind::Quote
    //   Kind::Normal
    nested_lines: Vec<SharedLine>,
    nested_blocks: Vec<Block>,
}

impl Line {
    fn new(ln: usize, line: String) -> Self {
        Line {
            num: ln,
            text: line,
            kind: Kind::PlainText,
            buff: vec![],
            nested_lines: vec![],
            nested_blocks: vec![],
        }
    }

    fn without_parse(&mut self, kind: Kind) {
        self.kind = kind;
    }

    // Parse a line of text into 'Line' struct that contains multi tokens.
    // Line's kind is determinded by the mark token's kind.
    fn parse(&mut self) {
        self.buff = Lexer::new(&self.text).split();

        self.kind = match self.mark_token().kind() {
            TokenKind::BlankLine => Kind::Blank,
            TokenKind::TitleMark => Kind::Title,
            TokenKind::UnorderedMark => Kind::UnorderedList,
            TokenKind::OrderedMark => Kind::OrderedList,
            TokenKind::DividingMark => Kind::Dividing,
            TokenKind::QuoteMark => Kind::Quote,
            TokenKind::CodeBlockMark => Kind::CodeBlockMark,
            _ => Kind::PlainText,
        };

        debug_assert!(!self.all().is_empty());
    }

    fn weak_parse(&self) -> Kind {
        if self.text().trim_start().starts_with("```") {
            Kind::CodeBlockMark
        } else {
            Kind::PlainText
        }
    }

    // Get number of the indent, two white space(' ') or one '\t' is a indent
    fn indents(&self) -> isize {
        let first = self.first_token();
        if first.kind() != TokenKind::WhiteSpace {
            return 0;
        }
        let sum: isize = first
            .value()
            .chars()
            .map(|c| if c == '\t' { 2 } else { 1 })
            .sum();
        sum / 2
    }

    // Determine whether the current line is a nested line of 'parent'
    // The return value is the number of nested indents, it's not a nested if less than or equal to 0.
    fn is_nested(&self, parent: &Line) -> isize {
        if self.kind == Kind::Blank
            || self.kind == Kind::Title
            || self.kind == Kind::Dividing
            || self.kind == Kind::CodeBlockMark
            || self.kind == Kind::CodeBlock
        {
            0
        } else {
            self.indents() - parent.indents()
        }
    }

    fn first_token(&self) -> &Token {
        &self.buff[0]
    }

    fn last_token(&self) -> &Token {
        self.all().last().unwrap_or_else(|| self.first_token())
    }

    fn pick_reflink_tags(&self, tags: &mut HashMap<String, (String, String)>) {
        for t in self
            .all()
            .iter()
            .filter(|t| t.kind() == TokenKind::RefLinkDef)
        {
            let gl = t.as_generic_link();
            let (tag, location, title) = (gl.tag(), gl.location(), gl.title());
            if !tag.is_empty() {
                tags.insert(tag.to_string(), (location.to_string(), title.to_string()));
            }
        }
    }

    fn meta() -> Self {
        let mut l = Self::new(0, "meta".to_string());
        l.without_parse(Kind::Meta__);
        l
    }

    pub(crate) fn enter_nested_blocks(&self, generator: &impl Generate) -> String {
        self.nested_blocks
            .iter()
            .filter(|b| {
                b.kind() != Kind::Meta__
                    && b.kind() != Kind::ListNesting__
                    && b.kind() != Kind::Blank
                    && b.kind() != Kind::Title
                    && b.kind() != Kind::Dividing
                    && b.kind() != Kind::CodeBlockMark
                    && b.kind() != Kind::CodeBlock
            })
            .map(|b| match b.kind() {
                Kind::Title => generator.render_title(b.first()),
                Kind::PlainText => generator.render_plain_text(b.contains()),
                Kind::Dividing => generator.render_dividing(b.first()),
                Kind::CodeBlock => generator.render_code(b.contains()),
                Kind::UnorderedList => generator.render_unordered_list(b.contains()),
                Kind::Blank => generator.render_blank(b.contains()),
                Kind::Quote => {
                    let s = b
                        .quote_ast
                        .as_ref()
                        .map(|a| a.generate_content(generator))
                        .unwrap_or_else(|| "".to_string());
                    generator.render_quote(&s)
                }
                Kind::OrderedList => generator.render_ordered_list(b.contains()),
                _ => "".to_string(),
            })
            .join("\n")
    }

    // Get the mark token in the Line, the mark token may be the first or second
    pub(crate) fn mark_token(&self) -> &Token {
        let first = self.first_token();
        if first.kind() == TokenKind::WhiteSpace {
            // if the first token is 'WhiteSpace', the second token must be exist
            &self.buff[1]
        } else {
            first
        }
    }

    // Get the Nth Token in the Line
    pub(crate) fn get(&self, at: usize) -> Option<&Token> {
        self.buff.get(at)
    }

    // Get all tokens in the Line
    pub(crate) fn all(&self) -> &Vec<Token> {
        &self.buff
    }

    // Get the line text
    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    pub(crate) fn html_escaped_text(&self) -> String {
        htmlescape::escape(self.text()).to_string()
    }

    // create a anchor name and id for the line
    pub(crate) fn anchor(&self) -> (String, String) {
        if self.kind != Kind::Title {
            panic!("Only title line can create anchor");
        }
        let ss: Vec<String> = self
            .all()
            .iter()
            .filter(|t| t.kind() != TokenKind::WhiteSpace && t.kind() != TokenKind::TitleMark)
            .map(|t| t.html_escaped_value())
            .collect();

        let name = ss.join("");
        (
            format!("{}-{}", &name.to_lowercase().replace(' ', "-"), self.num),
            name,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockGenerator {}
    impl Generate for MockGenerator {
        fn render_unordered_list(&self, ls: &[SharedLine]) -> String {
            let list: Vec<String> = ls
                .iter()
                .map(|l| {
                    let leader = l.borrow().text().trim().to_string();
                    let nesting = l.borrow().enter_nested_blocks(&MockGenerator {});
                    if !nesting.is_empty() {
                        leader + nesting.as_str()
                    } else {
                        leader
                    }
                })
                .map(|s| format!("<li>{}</li>", s))
                .collect();
            format!("<ul>{}</ul>", list.join(""))
        }
    }

    fn exec_document_cases(doc: &Vec<SharedLine>) -> Vec<(Kind, usize, usize, usize)> {
        doc.iter()
            .skip(1)
            .map(|x| {
                let x = x.borrow();
                (x.kind, x.num, x.nested_lines.len(), x.nested_blocks.len())
            })
            .collect()
    }

    fn exec_blocks_cases(blocks: &Vec<Block>) -> Vec<(Kind, usize, Option<usize>)> {
        blocks
            .iter()
            .map(|x| {
                (
                    x.kind(),
                    x.contains.len(),
                    x.quote_ast
                        .as_ref()
                        .map(|a| Some(a.count_lines()))
                        .unwrap_or(None),
                )
            })
            .collect()
    }

    #[test]
    fn test_code_block() {
        let md = r#"这是一个代码块的例子：
```
    let s = \"hello world\";
    let s1 = s.to_string();
```
    ```
    let s;
    assert_eq!(ast.doc[0].sequence[1].kind, TokenKind::Url);
    assert_eq!(ast.doc[0].sequence[1].value, url.to_string());"#;
        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        // (line kind, line number, nested line count, nested block count)
        let document = vec![
            (Kind::PlainText, 1, 0, 0),
            (Kind::CodeBlockMark, 2, 0, 0),
            (Kind::CodeBlock, 3, 0, 0),
            (Kind::CodeBlock, 4, 0, 0),
            (Kind::CodeBlockMark, 5, 0, 0),
            (Kind::CodeBlockMark, 6, 0, 0),
            (Kind::PlainText, 7, 0, 0),
            (Kind::PlainText, 8, 0, 0),
            (Kind::PlainText, 9, 0, 0),
        ];
        assert_eq!(exec_document_cases(&ast.document), document);

        // (block kind, line count in block, line count of quote ast)
        let blocks = vec![
            (Kind::PlainText, 1, None),
            (Kind::CodeBlock, 4, None),
            (Kind::CodeBlockMark, 1, None),
            (Kind::PlainText, 3, None),
        ];
        assert_eq!(exec_blocks_cases(ast.blocks()), blocks);
    }

    #[test]
    fn test_quote_block() {
        {
            let md = r#"# header1
> hello, rust
> hello, world
>> hello, nested

<user@gmail.com>
"#;

            let mut ast = Ast::new();
            ast.parse_string(md).unwrap();

            // (line kind, line number, nested line count, nested block count)
            let document = vec![
                (Kind::Title, 1, 0, 0),
                (Kind::Quote, 2, 0, 0),
                (Kind::Quote, 3, 0, 0),
                (Kind::Quote, 4, 0, 0),
                (Kind::Blank, 5, 0, 0),
                (Kind::PlainText, 6, 0, 0),
            ];
            assert_eq!(exec_document_cases(&ast.document), document);

            // (block kind, line count in block, line count of quote ast)
            let blocks = vec![
                (Kind::Title, 1, None),
                (Kind::Quote, 3, Some(3)),
                (Kind::Blank, 1, None),
                (Kind::PlainText, 1, None),
            ];
            assert_eq!(exec_blocks_cases(ast.blocks()), blocks);
        }
    }

    #[test]
    fn test_nested_list() {
        let md = r#"## 无序列表
- 列表项 1
    嵌入文本 1
  - 嵌入项 1
  - 嵌入项 2
- 列表项 2
- 列表项 3"#;
        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        let document = vec![
            (Kind::Title, 1, 0, 0),
            (Kind::UnorderedList, 2, 3, 2),
            (Kind::PlainText, 3, 0, 0),
            (Kind::UnorderedList, 4, 0, 0),
            (Kind::UnorderedList, 5, 0, 0),
            (Kind::UnorderedList, 6, 0, 0),
            (Kind::UnorderedList, 7, 0, 0),
        ];
        assert_eq!(exec_document_cases(&ast.document), document);

        // (block kind, line count in block, line count of quote ast)
        let blocks = vec![(Kind::Title, 1, None), (Kind::UnorderedList, 3, None)];
        assert_eq!(exec_blocks_cases(ast.blocks()), blocks);
    }

    #[test]
    fn test_ref_link_tags() {
        let md = r#"## 链接
[Example][link] <br>
[link]: https://www.example.com "example""#;
        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        assert_eq!(ast.ref_link_tags().len(), 1);
        assert_eq!(
            ast.ref_link_tags().get("link"),
            Some(&("https://www.example.com".to_string(), "example".to_string()))
        );
    }

    #[test]
    fn test_generate_toc() {
        let md = r#"
# header1
## header2
## header2
### header3
#### header4
## header2
"#;

        let dest = "<ul>\
<li>header1\
<ul>\
<li>header2</li>\
<li>header2\
<ul>\
<li>header3\
<ul>\
<li>header4</li>\
</ul>\
</li>\
</ul>\
</li>\
<li>header2</li>\
</ul>\
</li>\
</ul>";
        let mut ast = Ast::new();
        ast.parse_string(md).unwrap();

        assert_eq!(ast.toc.len(), 6);

        let s = ast.generate_toc(&MockGenerator {});
        assert_eq!(s, dest);
    }
}
