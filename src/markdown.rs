use std::error::Error;

use crate::config::Config;
use crate::html;
use crate::parser::Ast;

#[derive(Debug)]
pub struct Markdown<'markdown> {
    ast: Ast,
    config: Config,
    path: Option<&'markdown str>,
    text: Option<&'markdown str>,
}

impl<'markdown> Default for Markdown<'markdown> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'markdown> Markdown<'markdown> {
    pub fn new() -> Self {
        Markdown {
            ast: Ast::new(),
            config: Config::default(),
            path: None,
            text: None,
        }
    }

    // Setup the custom configure info
    pub fn config(&mut self, config: Config) -> &mut Self {
        self.config = config;
        self
    }

    // Specify the path of a markdown file, then read the file and parse it
    pub fn path(&mut self, path: &'markdown str) -> &mut Self {
        self.path = Some(path);
        self
    }

    // Provide the content of a markdown file, then parse it directly
    pub fn text(&mut self, text: &'markdown str) -> &mut Self {
        self.text = Some(text);
        self
    }

    // Use 'f' function to convert markdown ast into a string, .e.g html document
    pub fn map_mut<F>(&mut self, f: F) -> Result<String, Box<dyn Error>>
    where
        F: Fn(&Ast, &Config) -> Result<String, Box<dyn Error>>,
    {
        self.parse()?;
        let s = f(&self.ast, &self.config)?;
        Ok(s)
    }

    fn parse(&mut self) -> Result<&Self, Box<dyn Error>> {
        match self.text {
            Some(s) => self.ast.parse_string(s)?,
            None => match self.path {
                Some(p) => self.ast.parse_file(p)?,
                None => return Err("not found path or text to parse".into()),
            },
        }
        Ok(self)
    }
}

// Convert markdown ast into html
pub fn to_html(ast: &Ast, cfg: &Config) -> Result<String, Box<dyn Error>> {
    Ok(ast.generate_html(&html::Generator::new(cfg, ast.ref_link_tags())?))
}

// Convert markdown ast into body part of the html
pub fn to_html_body(ast: &Ast, cfg: &Config) -> Result<String, Box<dyn Error>> {
    Ok(ast.generate_body(&html::Generator::new(cfg, ast.ref_link_tags())?))
}

// Generate the toc part of the html from markdown ast
pub fn to_toc(ast: &Ast, cfg: &Config) -> Result<String, Box<dyn Error>> {
    Ok(ast.generate_toc(&html::Generator::new(cfg, ast.ref_link_tags())?))
}
