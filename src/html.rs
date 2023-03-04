use std::collections::HashMap;
use std::error::Error;

use super::SharedLine;
use crate::config::Config;
use crate::html_generate::HtmlGenerate;
use crate::lexer::{Token, TokenKind};
use crate::stack;

use serde::Serialize;
use tinytemplate::TinyTemplate;

pub(crate) struct Generator<'generator> {
    template: TinyTemplate<'generator>,
    cfg: &'generator Config,
    ref_link_tags: &'generator HashMap<String, (String, String)>,
}

impl<'generator> Generator<'generator> {
    pub(crate) fn new(
        cfg: &'generator Config,
        ref_link_tags: &'generator HashMap<String, (String, String)>,
    ) -> Result<Self, Box<dyn Error>> {
        let mut g = Generator {
            template: TinyTemplate::new(),
            cfg,
            ref_link_tags,
        };
        g.init()
            .map_err(|e| format!("failed to init the html generator: {}", e))?;
        Ok(g)
    }

    fn init(&mut self) -> Result<(), Box<dyn Error>> {
        let templates = vec![
            (TP_ORDERED_LIST_NAME, TP_ORDERED_LIST),
            (TP_UNORDERED_LIST_NAME, TP_UNORDERED_LIST),
            (TP_TITLE_NAME, TP_TITLE),
            (TP_QUOTE_NAME, TP_QUOTE),
            (TP_IMG_NAME, TP_IMG),
            (TP_LINK_NAME, TP_LINK),
            (TP_CODE_NAME, TP_CODE),
            (TP_PLAIN_TEXT_NAME, TP_PLAIN_TEXT),
            (TP_HEAD_NAME, TP_HEAD),
            (TP_BODY_BEGIN_NAME, TP_BODY_BEGIN),
        ];
        for (name, tp) in templates {
            self.template.add_template(name, tp)?;
        }
        self.template
            .set_default_formatter(&tinytemplate::format_unescaped);
        Ok(())
    }

    fn render_inline(&self, tokens: &Vec<Token>) -> String {
        let mut stack: stack::Stack<(TokenKind, &str)> = stack::Stack::new();
        let mut buff = String::new();

        for t in tokens {
            match t.kind() {
                TokenKind::Text | TokenKind::LineBreak => {
                    buff.push_str(t.html_escaped_value().as_str())
                }
                TokenKind::CodeMark => {
                    let matched = stack.pop_or_push((t.kind(), t.value()), |e| {
                        e.0 == t.kind() && e.1 == t.value()
                    });
                    if matched.is_some() {
                        buff.push_str("</code>");
                    } else {
                        buff.push_str("<code>");
                    }
                }
                TokenKind::BoldMark => {
                    let matched = stack.pop_or_push((t.kind(), t.value()), |e| {
                        e.0 == t.kind() && e.1 == t.value()
                    });
                    if matched.is_some() {
                        buff.push_str("</strong>");
                    } else {
                        buff.push_str("<strong>");
                    }
                }
                TokenKind::ItalicMark => {
                    let matched = stack.pop_or_push((t.kind(), t.value()), |e| {
                        e.0 == t.kind() && e.1 == t.value()
                    });
                    if matched.is_some() {
                        buff.push_str("</em>");
                    } else {
                        buff.push_str("<em>");
                    }
                }
                TokenKind::ItalicBoldMark => {
                    let matched = stack.pop_or_push((t.kind(), t.value()), |e| {
                        e.0 == t.kind() && e.1 == t.value()
                    });
                    if matched.is_some() {
                        buff.push_str("</em></strong>");
                    } else {
                        buff.push_str("<strong><em>");
                    }
                }
                TokenKind::DeleteMark => {
                    let matched = stack.pop_or_push((t.kind(), t.value()), |e| {
                        e.0 == t.kind() && e.1 == t.value()
                    });
                    if matched.is_some() {
                        buff.push_str("</del>");
                    } else {
                        buff.push_str("<del>");
                    }
                }
                TokenKind::Link | TokenKind::QuickLink | TokenKind::Image => {
                    let link = t.as_generic_link();
                    let (name, location) = (link.name(), link.location());

                    if !name.is_empty() && !location.is_empty() {
                        let s = if t.kind() == TokenKind::Image {
                            self.render_image(name, location)
                        } else {
                            self.render_link(name, location)
                        };
                        buff.push_str(&s);
                    }
                }
                TokenKind::RefLink => {
                    let link = t.as_generic_link();
                    let (name, tag) = (link.name(), link.tag());

                    let default = (String::from(""), String::from(""));
                    let (location, _title) = self.ref_link_tags.get(tag).unwrap_or(&default);
                    if !name.is_empty() && !location.is_empty() {
                        let s = self.render_link(name, location);
                        buff.push_str(&s);
                    }
                }
                TokenKind::UnorderedMark => match t.second_kind() {
                    Some(TokenKind::TodoDoneMark) => {
                        buff.push_str(r#"<input type="checkbox" disabled checked> "#);
                    }
                    Some(TokenKind::TodoUndoneMark) => {
                        buff.push_str(r#"<input type="checkbox" disabled> "#);
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        buff
    }

    fn render_link(&self, show_name: &str, location: &str) -> String {
        self.template
            .render(
                TP_LINK_NAME,
                &LinkContext {
                    show_name,
                    location,
                },
            )
            .unwrap()
    }

    fn render_image(&self, alt: &str, location: &str) -> String {
        self.template
            .render(TP_IMG_NAME, &ImageContext { alt, location })
            .unwrap()
    }
}

impl<'generator> HtmlGenerate for Generator<'generator> {
    fn head(&self) -> String {
        if !self.cfg.custom_html_head.is_empty() {
            return self.cfg.custom_html_head.clone();
        }
        let ctx = HeadContext {
            css_href: &self.cfg.css_href,
        };
        self.template.render(TP_HEAD_NAME, &ctx).unwrap()
    }

    fn body_begin(&self) -> String {
        let ctx = BodyBeginContext {
            article_class: &self.cfg.add_class_on_article,
        };
        self.template.render(TP_BODY_BEGIN_NAME, &ctx).unwrap()
    }

    fn body_end(&self) -> String {
        String::from("</article></body>")
    }

    fn body_title(&self, l: &SharedLine) -> String {
        let l = l.borrow();
        let level = l.mark_token().len();
        let value = self.render_inline(l.all());

        let ctx = TitleContext {
            is_l1: level == 1,
            is_l2: level == 2,
            is_l3: level == 3,
            is_l4: level == 4,
            is_l5: level == 5,
            is_l6: level == 6,
            text: value,
        };

        self.template.render(TP_TITLE_NAME, &ctx).unwrap()
    }

    fn body_dividing(&self, _l: &SharedLine) -> String {
        String::from("<hr>")
    }

    fn body_plain_text(&self, ls: &[SharedLine]) -> String {
        let mut lines: Vec<String> = ls
            .iter()
            .map(|l| {
                let mut s = self.render_inline(l.borrow().all());
                if self.cfg.enable_newline_to_br && !s.ends_with("<br>") {
                    s.push_str("<br>");
                }
                s
            })
            .collect();

        // remove the trailing '<br>' of the last element
        // TODO: optimize
        if let Some(last) = lines.pop() {
            lines.push(last.trim_end_matches("<br>").to_string());
        }

        self.template
            .render(TP_PLAIN_TEXT_NAME, &PlainTextContext { lines })
            .unwrap()
    }

    fn body_blank(&self, _ls: &[SharedLine]) -> String {
        String::from("")
    }

    fn body_ordered_list(&self, ls: &[SharedLine]) -> String {
        let list: Vec<String> = ls
            .iter()
            .map(|l| {
                let leader = self.render_inline(l.borrow().all());
                let nesting = l
                    .borrow()
                    .enter_nested_blocks(&Generator::new(self.cfg, self.ref_link_tags).unwrap());
                if !nesting.is_empty() {
                    leader + "\n" + nesting.as_str()
                } else {
                    leader
                }
            })
            .collect();
        self.template
            .render(TP_ORDERED_LIST_NAME, &OrderedListContext { list })
            .unwrap()
    }

    fn body_unordered_list(&self, ls: &[SharedLine]) -> String {
        let list = ls
            .iter()
            .map(|l| {
                let leader = self.render_inline(l.borrow().all());
                let nesting = l
                    .borrow()
                    .enter_nested_blocks(&Generator::new(self.cfg, self.ref_link_tags).unwrap());
                if !nesting.is_empty() {
                    leader + "\n" + nesting.as_str()
                } else {
                    leader
                }
            })
            .collect();
        self.template
            .render(TP_UNORDERED_LIST_NAME, &UnorderedListContext { list })
            .unwrap()
    }

    fn body_quote(&self, s: &str) -> String {
        self.template
            .render(TP_QUOTE_NAME, &QuoteContext { text: s })
            .unwrap()
    }

    fn body_code(&self, ls: &[SharedLine]) -> String {
        debug_assert!(ls.len() >= 2);

        let first = &ls[0];
        let text: String = ls[1..ls.len() - 1] // skip the first and last elements
            .iter()
            .map(|l| l.borrow().html_escaped_text())
            .collect();

        self.template
            .render(
                TP_CODE_NAME,
                &CodeBlockContext {
                    name: first.borrow().get(1).map(|t| t.value()).unwrap_or(""),
                    text: &text,
                },
            )
            .unwrap()
    }
}

// header
const TP_HEAD_NAME: &str = "header";
const TP_HEAD: &str = r#"
<head>
<meta charset='UTF-8'><meta name='viewport' content='width=device-width initial-scale=1'>
{{ if css_href }} <link rel="stylesheet" type="text/css" href="{ css_href }"> {{ endif }}
<style>
        body\{
                box-sizing: border-box;
                min-width: 200px;
                max-width: 900px;
                margin: 0 auto;
                padding: 45px;
        }

        @media (max-width: 767px) \{
                body\{
                        padding: 15px;
                }
        }
</style>
<title></title>
</head>
"#;

#[derive(Serialize)]
struct HeadContext<'head_context> {
    css_href: &'head_context str,
}

const TP_BODY_BEGIN_NAME: &str = "body_begin";
const TP_BODY_BEGIN: &str = r#"
<body>
<article{{ if article_class }} class={article_class}{{ endif }}>
"#;

#[derive(Serialize)]
struct BodyBeginContext<'body_begin_context> {
    article_class: &'body_begin_context str,
}

// title
const TP_TITLE_NAME: &str = "title";
const TP_TITLE: &str = "\
{{ if is_l1 }}<h1>{text}</h1>{{ endif }}\
{{ if is_l2 }}<h2>{text}</h2>{{ endif }}\
{{ if is_l3 }}<h3>{text}</h3>{{ endif }}\
{{ if is_l4 }}<h4>{text}</h4>{{ endif }}\
{{ if is_l5 }}<h5>{text}</h5>{{ endif }}\
{{ if is_l6 }}<h6>{text}</h6>{{ endif }}";

#[derive(Serialize)]
struct TitleContext {
    is_l1: bool,
    is_l2: bool,
    is_l3: bool,
    is_l4: bool,
    is_l5: bool,
    is_l6: bool,
    text: String,
}

// ordered list
const TP_ORDERED_LIST_NAME: &str = "ordered_list";
const TP_ORDERED_LIST: &str = "\
<ol>\
{{ for item in list }}
    <li>{item}</li>\
{{ endfor }} 
</ol>";

#[derive(Serialize)]
struct OrderedListContext {
    list: Vec<String>,
}

// unordered list
const TP_UNORDERED_LIST_NAME: &str = "unordered_list";
const TP_UNORDERED_LIST: &str = "\
<ul>\
{{ for item in list }} 
    <li>{item}</li>\
{{ endfor }} 
</ul>";

#[derive(Serialize)]
struct UnorderedListContext {
    list: Vec<String>,
}

// link
const TP_LINK_NAME: &str = "link";
const TP_LINK: &str = r#"<a href="{location}">{show_name}</a>"#;

#[derive(Serialize)]
struct LinkContext<'link_context> {
    show_name: &'link_context str,
    location: &'link_context str,
}

// image
const TP_IMG_NAME: &str = "img";
const TP_IMG: &str = r#"<img src="{location}" alt="{alt}">"#;

#[derive(Serialize)]
struct ImageContext<'image_context> {
    alt: &'image_context str,
    location: &'image_context str,
}

// code block
const TP_CODE_NAME: &str = "code_block";
const TP_CODE: &str = "<pre><code>\
{text}\
</code></pre>";

#[derive(Serialize)]
struct CodeBlockContext<'code_block_context> {
    name: &'code_block_context str,
    text: &'code_block_context str,
}

// plain text
const TP_PLAIN_TEXT_NAME: &str = "plain_text";
const TP_PLAIN_TEXT: &str = "\
<p>\
{{ for text in lines}}\
{text}\
{{ endfor }}\
</p>";

#[derive(Serialize)]
struct PlainTextContext {
    lines: Vec<String>,
}

// quote block
const TP_QUOTE_NAME: &str = "quote";
const TP_QUOTE: &str = "\
<blockquote><p>
    {text}
</p></blockquote>";

#[derive(Serialize)]
struct QuoteContext<'quote_context> {
    text: &'quote_context str,
}
