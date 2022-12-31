use crate::html;
use crate::parser::{Ast, Kind};
use std::error::Error;

// Parse a markdown file and then render it into html
pub fn process_file(path: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = Ast::new();
    ast.parse_file(path)?;
    process(&ast)
}

// Parse a markdown string and then render it into html
pub fn process_string(s: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = Ast::new();
    ast.parse_string(s)?;
    process(&ast)
}

fn process(ast: &Ast) -> Result<String, Box<dyn Error>> {
    let html_doc = ast.block_to_string(|kind, ls| {
        let res = match kind {
            Kind::Title => Some(html::gen_title(ls)?),
            Kind::CodeMark => None,
            Kind::Code => None,
            Kind::DisorderedList => Some(html::gen_disordered_list(ls)?),
            Kind::DividingLine => Some(html::gen_dividling()?),
            Kind::Blank | Kind::NormalText => Some(html::gen_normal(ls)?),
            Kind::Quote => Some(html::gen_quote(ls)?),
            Kind::SortedList => Some(html::gen_sorted_list(ls)?),
        };
        if let Some(res) = res {
            Ok(res)
        } else {
            Ok("".to_string())
        }
    })?;

    Ok(html_doc.join("\n"))
}
