use super::SharedLine;

pub(crate) trait HtmlGenerate {
    fn head(&self) -> String;
    fn body_begin(&self) -> String;
    fn body_end(&self) -> String;
    fn body_title(&self, l: &SharedLine) -> String;
    fn body_dividing(&self, l: &SharedLine) -> String;
    fn body_plain_text(&self, ls: &[SharedLine]) -> String;
    fn body_blank(&self, ls: &[SharedLine]) -> String;
    fn body_ordered_list(&self, ls: &[SharedLine]) -> String;
    fn body_unordered_list(&self, ls: &[SharedLine]) -> String;
    fn body_quote(&self, s: &str) -> String;
    fn body_code(&self, ls: &[SharedLine]) -> String;
}
