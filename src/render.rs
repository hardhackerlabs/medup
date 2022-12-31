use crate::html;
use crate::parser::{Ast, Kind};
use std::error::Error;

pub fn process_file(path: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = Ast::new();
    ast.parse_file(path)?;
    process(&ast)
}

pub fn process_string(s: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = Ast::new();
    ast.parse_string(s)?;
    process(&ast)
}

fn process(ast: &Ast) -> Result<String, Box<dyn Error>> {
    let mut html_doc: Vec<String> = Vec::new();

    ast.for_each_block(&mut html_doc, |out, kind, ls| {
        let res = match kind {
            Kind::Title => {
                if let Some(l) = ls.first() {
                    Some(html::gen_title(l)?)
                } else {
                    None
                }
            }
            Kind::CodeMark => None,
            Kind::Code => None,
            Kind::DisorderedList => Some(html::gen_disordered_list(ls)?),
            Kind::DividingLine => Some(html::gen_dividling()?),
            Kind::Blank | Kind::NormalText => Some(html::gen_normal(ls)?),
            Kind::Quote => Some(html::gen_quote(ls)?),
            Kind::SortedList => Some(html::gen_sorted_list(ls)?),
        };
        if let Some(res) = res {
            out.push(res);
        }
        Ok(())
    })?;

    Ok(html_doc.join("\n"))
}
