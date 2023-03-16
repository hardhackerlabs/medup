use super::SharedLine;

pub trait Generate {
    fn head(&self) -> String {
        String::from(
            r#"<head>
<meta charset='UTF-8'><meta name='viewport' content='width=device-width initial-scale=1'>
<title></title>
</head>"#,
        )
    }

    fn body_begin(&self) -> String {
        String::from("<body>")
    }

    fn body_end(&self) -> String {
        String::from("</body>")
    }

    fn body_title(&self, l: &SharedLine) -> String {
        l.borrow().text().trim().to_string()
    }

    fn body_dividing(&self, _l: &SharedLine) -> String {
        "".to_string()
    }

    fn body_plain_text(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn body_blank(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn body_ordered_list(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn body_unordered_list(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }

    fn body_quote(&self, _s: &str) -> String {
        "".to_string()
    }

    fn body_code(&self, _ls: &[SharedLine]) -> String {
        "".to_string()
    }
}
