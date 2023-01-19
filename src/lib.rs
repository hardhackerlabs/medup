use std::cell::RefCell;
use std::rc::Rc;

pub mod config;
mod cursor;
mod html;
mod html_generate;
mod lexer;
pub mod markdown;
mod parser;
mod stack;

type SharedLine = Rc<RefCell<parser::Line>>;
