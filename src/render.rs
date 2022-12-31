use crate::html;
use crate::parser;
use std::error::Error;

// Parse a markdown file and then render it into html
pub fn process_file(path: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = parser::Ast::new();
    ast.parse_file(path)?;
    let mut gen = html::Generator::new();
    gen.init()?;
    ast.to_html(&gen)
}

// Parse a markdown string and then render it into html
pub fn process_string(s: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = parser::Ast::new();
    ast.parse_string(s)?;
    let mut gen = html::Generator::new();
    gen.init()?;
    ast.to_html(&gen)
}
