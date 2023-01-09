use serde::Serialize;

use std::error::Error;
use tinytemplate::TinyTemplate;

use itertools::Itertools;

use crate::parser::{HtmlGenerate, SharedLine, Token, TokenKind};

const TITLE_TEMPLATE_NAME: &str = "title";
const TITLE_TEMPLATE: &str = "{{ if is_l1 }}<h1>{text}</h1>{{ endif }}
{{ if is_l2 }}<h2>{text}</h2>{{ endif }}
{{ if is_l3 }}<h3>{text}</h3>{{ endif }}
{{ if is_l4 }}<h4>{text}</h4>{{ endif }}";

const URL_TEMPLATE_NAME: &str = "url";
const URL_TEMPLATE: &str = "<a href=\"{location}\">{show_name}</a>";

const IMG_TEMPLATE_NAME: &str = "img";
const IMG_TEMPLATE: &str = "<img src=\"{location}\" alt=\"{alt}\">";

const SORTED_LIST_TEMPLATE_NAME: &str = "sorted_list";
const SORTED_LIST_TEMPLATE: &str = "<ol> 
    {{ for item in list }} 
    <li>{item}</li> 
    {{ endfor }} 
</ol>";

const DISORDERED_LIST_TEMPLATE_NAME: &str = "disordered_list";
const DISORDERED_LIST_TEMPLATE: &str = "<ul> 
    {{ for item in list }} 
    <li>{item}</li> 
    {{ endfor }} 
</ul>";

const QUOTE_TEMPLATE_NAME: &str = "quote";
const QUOTE_TEMPLATE: &str = "<blockquote> 
    {{ for text in lines }} 
    <p>{text}</p>
    {{ endfor }} 
</blockquote>";

const CODE_TEMPLATE_NAME: &str = "code_block";
const CODE_TEMPLATE: &str = "<pre><code class=\"language-{name}\">
{text}
</code></pre>";

const TEXT_PARAGRAPH_TEMPLATE_NAME: &str = "paragraph";
const TEXT_PARAGRAPH_TEMPLATE: &str = "<p>{text}</p>";

#[derive(Serialize)]
struct TitleContext {
    is_l1: bool,
    is_l2: bool,
    is_l3: bool,
    is_l4: bool,
    text: String,
}

#[derive(Serialize)]
struct SortedListContext {
    list: Vec<String>,
}

#[derive(Serialize)]
struct DisorderedListContext {
    list: Vec<String>,
}

#[derive(Serialize)]
struct QuoteContext {
    lines: Vec<String>,
}

#[derive(Serialize)]
struct UrlContext<'uc> {
    show_name: &'uc str,
    location: &'uc str,
}

#[derive(Serialize)]
struct ImageContext<'ic> {
    alt: &'ic str,
    location: &'ic str,
}

#[derive(Serialize)]
struct CodeBlockContext<'cbc> {
    name: &'cbc str,
    text: &'cbc str,
}

#[derive(Serialize)]
struct TextParagraphContext<'tpc> {
    text: &'tpc str,
}

pub struct Generator<'generator> {
    tt: TinyTemplate<'generator>,
}

impl<'generator> Generator<'generator> {
    pub fn new() -> Result<Self, Box<dyn Error>> {
        let mut g = Generator {
            tt: TinyTemplate::new(),
        };
        g.init()?;
        Ok(g)
    }

    fn init(&mut self) -> Result<(), Box<dyn Error>> {
        self.tt.add_template(URL_TEMPLATE_NAME, URL_TEMPLATE)?;
        self.tt
            .add_template(SORTED_LIST_TEMPLATE_NAME, SORTED_LIST_TEMPLATE)?;
        self.tt
            .add_template(DISORDERED_LIST_TEMPLATE_NAME, DISORDERED_LIST_TEMPLATE)?;
        self.tt.add_template(TITLE_TEMPLATE_NAME, TITLE_TEMPLATE)?;
        self.tt.add_template(QUOTE_TEMPLATE_NAME, QUOTE_TEMPLATE)?;
        self.tt.add_template(IMG_TEMPLATE_NAME, IMG_TEMPLATE)?;
        self.tt.add_template(CODE_TEMPLATE_NAME, CODE_TEMPLATE)?;
        self.tt
            .add_template(TEXT_PARAGRAPH_TEMPLATE_NAME, TEXT_PARAGRAPH_TEMPLATE)?;

        self.tt
            .set_default_formatter(&tinytemplate::format_unescaped);
        Ok(())
    }

    fn gen_line(&self, tokens: &Vec<Token>) -> String {
        let mut text = String::new();
        let mut opened = false;
        for t in tokens {
            match t.kind() {
                TokenKind::Text => text.push_str(t.value()),
                TokenKind::BoldMark => {
                    text.push_str(if opened { "</strong>" } else { "<strong>" });
                    opened = !opened;
                }
                TokenKind::ItalicMark => {
                    text.push_str(if opened { "</em>" } else { "<em>" });
                    opened = !opened;
                }
                TokenKind::ItalicBoldMark => {
                    text.push_str(if opened {
                        "</em></strong>"
                    } else {
                        "<strong><em>"
                    });
                    opened = !opened;
                }
                TokenKind::Url => {
                    if let (Some(show_name), Some(location)) = (t.get_show_name(), t.get_location())
                    {
                        let s = self.gen_url(show_name, location);
                        text.push_str(&s);
                    }
                }
                TokenKind::Image => {
                    if let (Some(alt), Some(location)) = (t.get_show_name(), t.get_location()) {
                        let s = self.gen_image(alt, location);
                        text.push_str(&s);
                    }
                }
                _ => (),
            }
        }
        text
    }

    fn gen_url(&self, show_name: &str, location: &str) -> String {
        self.tt
            .render(
                URL_TEMPLATE_NAME,
                &UrlContext {
                    show_name,
                    location,
                },
            )
            .unwrap()
    }

    fn gen_image(&self, alt: &str, location: &str) -> String {
        self.tt
            .render(IMG_TEMPLATE_NAME, &ImageContext { alt, location })
            .unwrap()
    }
}

impl<'generator> HtmlGenerate for Generator<'generator> {
    fn gen_title(&self, l: &SharedLine) -> String {
        let l = l.borrow();
        let level = l.get_mark().len();
        let value = self.gen_line(l.all());

        let ctx = TitleContext {
            is_l1: level == 1,
            is_l2: level == 2,
            is_l3: level == 3,
            is_l4: level == 4,
            text: value,
        };

        self.tt.render(TITLE_TEMPLATE_NAME, &ctx).unwrap()
    }

    fn gen_dividling(&self, _l: &SharedLine) -> String {
        "<hr>".to_string()
    }

    fn gen_normal(&self, l: &SharedLine) -> String {
        let text = self.gen_line(l.borrow().all());
        self.tt
            .render(
                TEXT_PARAGRAPH_TEMPLATE_NAME,
                &TextParagraphContext { text: &text },
            )
            .unwrap()
    }

    fn gen_blank(&self, ls: &[SharedLine]) -> String {
        ls.iter().map(|_| "<p></p>").collect()
    }

    fn gen_sorted_list(&self, ls: &[SharedLine]) -> String {
        let list: Vec<String> = ls.iter().map(|l| self.gen_line(l.borrow().all())).collect();
        self.tt
            .render(SORTED_LIST_TEMPLATE_NAME, &SortedListContext { list })
            .unwrap()
    }

    fn gen_disordered_list(&self, ls: &[SharedLine]) -> String {
        let list = ls.iter().map(|l| self.gen_line(l.borrow().all())).collect();
        self.tt
            .render(
                DISORDERED_LIST_TEMPLATE_NAME,
                &DisorderedListContext { list },
            )
            .unwrap()
    }

    fn gen_quote(&self, ls: &[SharedLine]) -> String {
        let lines: Vec<String> = ls.iter().map(|l| self.gen_line(l.borrow().all())).collect();
        self.tt
            .render(QUOTE_TEMPLATE_NAME, &QuoteContext { lines })
            .unwrap()
    }

    fn gen_code(&self, ls: &[SharedLine]) -> String {
        debug_assert!(ls.len() >= 2);

        let text: String = ls[1..ls.len() - 1] // skip the first and last elements
            .iter()
            .map(|l| l.borrow().text().to_string())
            .collect(); // TODO: optimize to_string()
        self.tt
            .render(
                CODE_TEMPLATE_NAME,
                &CodeBlockContext {
                    name: "",
                    text: &text,
                },
            )
            .unwrap()
    }
}
