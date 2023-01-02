use serde::Serialize;

use std::error::Error;
use tinytemplate::TinyTemplate;

use crate::parser;

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
    {text}<br> 
    {{ endfor }} 
</blockquote>";

const CODE_TEMPLATE_NAME: &str = "code_block";
const CODE_TEMPLATE: &str = "<pre><code class=\"language-{name}\">
{text}
</code></pre>";

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

pub struct Generator<'generator> {
    tt: TinyTemplate<'generator>,
}

impl<'generator> Generator<'generator> {
    pub fn new() -> Self {
        Generator {
            tt: TinyTemplate::new(),
        }
    }

    pub fn init(&mut self) -> Result<(), Box<dyn Error>> {
        self.tt.add_template(URL_TEMPLATE_NAME, URL_TEMPLATE)?;
        self.tt
            .add_template(SORTED_LIST_TEMPLATE_NAME, SORTED_LIST_TEMPLATE)?;
        self.tt
            .add_template(DISORDERED_LIST_TEMPLATE_NAME, DISORDERED_LIST_TEMPLATE)?;
        self.tt.add_template(TITLE_TEMPLATE_NAME, TITLE_TEMPLATE)?;
        self.tt.add_template(QUOTE_TEMPLATE_NAME, QUOTE_TEMPLATE)?;
        self.tt.add_template(IMG_TEMPLATE_NAME, IMG_TEMPLATE)?;
        self.tt.add_template(CODE_TEMPLATE_NAME, CODE_TEMPLATE)?;
        Ok(())
    }

    fn gen_line(&self, tokens: &Vec<parser::Token>) -> Result<String, Box<dyn Error>> {
        let mut text = String::new();
        let mut in_bold = false;
        for t in tokens {
            match t.kind() {
                parser::TokenKind::Text => text.push_str(t.value()),
                parser::TokenKind::BoldMark => {
                    if in_bold {
                        text.push_str("</strong>");
                    } else {
                        text.push_str("<strong>");
                    }
                    in_bold = !in_bold;
                }
                parser::TokenKind::Url => {
                    if let (Some(show_name), Some(location)) = (t.get_show_name(), t.get_location())
                    {
                        let s = self.gen_url(show_name, location)?;
                        text.push_str(&s);
                    }
                }
                parser::TokenKind::Image => {
                    if let (Some(alt), Some(location)) = (t.get_show_name(), t.get_location()) {
                        let s = self.gen_image(alt, location)?;
                        text.push_str(&s);
                    }
                }
                _ => (),
            }
        }
        Ok(text)
    }

    fn gen_url(&self, show_name: &str, location: &str) -> Result<String, Box<dyn Error>> {
        let s = self.tt.render(
            URL_TEMPLATE_NAME,
            &UrlContext {
                show_name,
                location,
            },
        )?;
        Ok(s)
    }

    fn gen_image(&self, alt: &str, location: &str) -> Result<String, Box<dyn Error>> {
        let s = self
            .tt
            .render(IMG_TEMPLATE_NAME, &ImageContext { alt, location })?;
        Ok(s)
    }
}

impl<'generator> parser::HtmlGenerate for Generator<'generator> {
    fn gen_title(&self, l: &parser::Line) -> Result<String, Box<dyn Error>> {
        let first = l
            .first_token()
            .ok_or("not found the first mark token in a title line")?;
        let level = first.len();
        let value = self.gen_line(l.all_tokens())?;

        let ctx = TitleContext {
            is_l1: level == 1,
            is_l2: level == 2,
            is_l3: level == 3,
            is_l4: level == 4,
            text: value,
        };

        let s = self.tt.render(TITLE_TEMPLATE_NAME, &ctx)?;
        Ok(s)
    }

    fn gen_dividling(&self, _l: &parser::Line) -> Result<String, Box<dyn Error>> {
        Ok("<hr>".to_string())
    }

    fn gen_normal(&self, l: &parser::Line) -> Result<String, Box<dyn Error>> {
        let mut s = String::new();
        let text = self.gen_line(l.all_tokens())?;
        s.push_str("<p>");
        s.push_str(&text);
        s.push_str("</p>");
        Ok(s)
    }

    fn gen_blank(&self, ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
        let mut s = String::new();
        for _ in ls {
            s.push_str("<p></p>");
        }
        Ok(s)
    }

    fn gen_sorted_list(&self, ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
        let mut list = Vec::new();
        for l in ls {
            let value = self.gen_line(l.all_tokens())?;
            list.push(value);
        }
        let s = self
            .tt
            .render(SORTED_LIST_TEMPLATE_NAME, &SortedListContext { list })?;
        Ok(s)
    }

    fn gen_disordered_list(&self, ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
        let mut list = Vec::new();
        for l in ls {
            let value = self.gen_line(l.all_tokens())?;
            list.push(value);
        }
        let s = self.tt.render(
            DISORDERED_LIST_TEMPLATE_NAME,
            &DisorderedListContext { list },
        )?;
        Ok(s)
    }

    fn gen_quote(&self, ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
        let mut lines = Vec::new();
        for l in ls {
            let value = self.gen_line(l.all_tokens())?;
            lines.push(value);
        }
        let s = self
            .tt
            .render(QUOTE_TEMPLATE_NAME, &QuoteContext { lines })?;
        Ok(s)
    }

    fn gen_code(&self, ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
        let mut text = String::new();
        for l in ls {
            text.push_str(l.text());
        }
        let s = self.tt.render(
            CODE_TEMPLATE_NAME,
            &CodeBlockContext {
                name: "rust",
                text: &text,
            },
        )?;
        Ok(s)
    }
}
