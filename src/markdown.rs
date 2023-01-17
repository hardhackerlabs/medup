use std::error::Error;

use crate::config::Config;
use crate::html;
use crate::parser;

// Parse a markdown file and then render it into html
pub fn file_to_html(cfg: &Config, path: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = parser::Ast::new();
    ast.parse_file(path)?;
    Ok(ast.render_html(&html::Generator::new(cfg, ast.ref_link_tags())?))
}

// Parse a markdown string and then render it into html
pub fn to_html(cfg: &Config, s: &str) -> Result<String, Box<dyn Error>> {
    let mut ast = parser::Ast::new();
    ast.parse_string(s)?;
    Ok(ast.render_html(&html::Generator::new(cfg, ast.ref_link_tags())?))
}
