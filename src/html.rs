use serde::Serialize;

use std::error::Error;
use tinytemplate::TinyTemplate;

use crate::parser::{self, Token};

// Title
static TITLE_TEMPLATE: &str = "{{ if is_l1 }}<h1>{text}</h1>{{ endif }}
{{ if is_l2 }}<h2>{text}</h2>{{ endif }}
{{ if is_l3 }}<h3>{text}</h3>{{ endif }}
{{ if is_l4 }}<h4>{text}</h4>{{ endif }}";

#[derive(Serialize)]
struct TitleContext {
    is_l1: bool,
    is_l2: bool,
    is_l3: bool,
    is_l4: bool,
    text: String,
}

pub fn gen_title(ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
    let l = ls.first().unwrap();
    let first = l.first_token().unwrap();
    let level = first.len();
    let value = gen_line(l.all_tokens())?;

    let ctx = TitleContext {
        is_l1: level == 1,
        is_l2: level == 2,
        is_l3: level == 3,
        is_l4: level == 4,
        text: value,
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
struct SortedListContext {
    list: Vec<String>,
}

pub fn gen_sorted_list(ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
    let mut list = Vec::new();
    for l in ls {
        let value = gen_line(l.all_tokens())?;
        list.push(value);
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
struct DisorderedListContext {
    list: Vec<String>,
}

pub fn gen_disordered_list(ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
    let mut list = Vec::new();
    for l in ls {
        let value = gen_line(l.all_tokens())?;
        list.push(value);
    }
    let ctx = DisorderedListContext { list };
    let mut tt = TinyTemplate::new();
    tt.add_template("disorded_list", DISORDERED_LIST_TEMPLATE)?;
    let s = tt.render("disorded_list", &ctx)?;
    Ok(s)
}

// Normal Text
pub fn gen_normal(ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
    let mut s = String::new();
    for l in ls {
        let text = gen_line(l.all_tokens())?;
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
struct QuoteContext {
    lines: Vec<String>,
}

pub fn gen_quote(ls: Vec<&parser::Line>) -> Result<String, Box<dyn Error>> {
    let mut lines = Vec::new();
    for l in ls {
        let value = gen_line(l.all_tokens())?;
        lines.push(value);
    }
    let mut tt = TinyTemplate::new();
    tt.add_template("quote", QUOTE_TEMPLATE)?;
    let s = tt.render("quote", &QuoteContext { lines })?;
    Ok(s)
}

// Dividling
pub fn gen_dividling() -> Result<String, Box<dyn Error>> {
    Ok("<hr>".to_string())
}

#[derive(Serialize)]
struct CodeBlockContext {}

pub fn gen_code() -> Result<(), Box<dyn Error>> {
    Ok(())
}

// Url
static URL_TEMPLATE: &str = "<a href=\"{location}\">{show_name}</a>";

#[derive(Serialize)]
struct UrlContext<'uc> {
    show_name: &'uc str,
    location: &'uc str,
}

pub fn gen_url(show_name: &str, location: &str) -> Result<String, Box<dyn Error>> {
    let mut tt = TinyTemplate::new();
    tt.add_template("url", URL_TEMPLATE)?;
    let s = tt.render(
        "url",
        &UrlContext {
            show_name,
            location,
        },
    )?;
    Ok(s)
}

// Image

#[derive(Serialize)]
struct ImageContext {}

pub fn gen_image() -> Result<(), Box<dyn Error>> {
    Ok(())
}

fn gen_line(tokens: &Vec<Token>) -> Result<String, Box<dyn Error>> {
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
                if let (Some(show_name), Some(addr)) = (t.get_show_name(), t.get_location()) {
                    let s = gen_url(show_name, addr)?;
                    text.push_str(&s);
                }
            }
            parser::TokenKind::Image => {} // TODO:
            _ => (),
        }
    }
    Ok(text)
}
