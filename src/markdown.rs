use crate::html;
use crate::parser;
use std::error::Error;

// Parse a markdown file and then render it into html
pub fn file_to_html(path: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = parser::Ast::new();
    ast.parse_file(path)?;
    let mut gen = html::Generator::new();
    gen.init()?;
    ast.to_html(&gen)
}

// Parse a markdown string and then render it into html
pub fn to_html(s: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = parser::Ast::new();
    ast.parse_string(s)?;
    let mut gen = html::Generator::new();
    gen.init()?;
    ast.to_html(&gen)
}
