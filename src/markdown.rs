use std::error::Error;

use crate::config::Config;
use crate::html;
use crate::parser::Ast;

pub struct Markdown<'markdown> {
    ast: Ast,
    cfg: &'markdown Config,
}

impl<'markdown> Markdown<'markdown> {
    pub fn new(cfg: &'markdown Config) -> Self {
        Markdown {
            ast: Ast::new(),
            cfg,
        }
    }

    // Parse a markdown file and then render it into html
    pub fn file_to_html(&mut self, path: &str) -> Result<String, Box<dyn Error>> {
        self.ast.parse_file(path)?;
        let gen = html::Generator::new(self.cfg, self.ast.ref_link_tags())?;
        Ok(self.ast.render_html(&gen))
    }

    // Parse a markdown string and then render it into html
    pub fn to_html(&mut self, s: &str) -> Result<String, Box<dyn Error>> {
        self.ast.parse_string(s)?;
        let gen = html::Generator::new(self.cfg, self.ast.ref_link_tags())?;
        Ok(self.ast.render_html(&gen))
    }

    // Parse a markdown string and generate raw html body
    pub fn to_raw(&mut self, s: &str) -> Result<String, Box<dyn Error>> {
        self.ast.parse_string(s)?;
        let gen = html::Generator::new(self.cfg, self.ast.ref_link_tags())?;
        Ok(self.ast.render_html_body(&gen))
    }
}
