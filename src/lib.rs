use std::cell::RefCell;
use std::rc::Rc;

mod html;
mod lexer;
pub mod markdown;
mod parser;
pub mod utils;

pub type SharedLine = Rc<RefCell<parser::Line>>;

pub trait Generate {
    fn render_title(&self, l: &SharedLine) -> String {
        l.borrow().text().trim().to_string()
    }

    fn render_dividing(&self, _l: &SharedLine) -> String {
        "".to_string()
    }

    fn render_plain_text(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn render_blank(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn render_ordered_list(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn render_unordered_list(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn render_quote(&self, _s: &str) -> String {
        "".to_string()
    }

    fn render_code(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }
}
