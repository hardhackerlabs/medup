use std::error::Error;

use serde::Serialize;
use tinytemplate::TinyTemplate;

use crate::config::Config;

const TPL_HTML_NAME: &str = "template";

#[derive(Serialize)]
struct HtmlContext<'html_context> {
    title: &'html_context str,
    css_href: &'html_context str,
    body_min_width: i32,
    body_max_width: i32,
    use_slice_mode: bool,
    slice_header: &'html_context str,
    content: &'html_context str,
    slices: &'html_context Vec<String>,
}

pub(crate) struct RenderHtml<'render_html> {
    tt: TinyTemplate<'render_html>,
}

impl<'render_html> RenderHtml<'render_html> {
    pub(crate) fn new(template: &'render_html str) -> Result<Self, Box<dyn Error>> {
        let mut tt = TinyTemplate::new();
        tt.add_template(TPL_HTML_NAME, template)?;
        tt.set_default_formatter(&tinytemplate::format_unescaped);
        Ok(RenderHtml { tt })
    }

    pub(crate) fn exec(&self, cfg: &Config, data: &Vec<String>) -> Result<String, Box<dyn Error>> {
        let content = if !cfg.use_slice_mode() {
            data.join("")
        } else {
            "".to_string()
        };

        let ctx = HtmlContext {
            title: "medup",
            css_href: cfg.css_href(),
            body_min_width: cfg.body_min_width(),
            body_max_width: cfg.body_max_width(),
            use_slice_mode: cfg.use_slice_mode(),
            slice_header: &cfg.slice_header(),
            content: &content,
            slices: data,
        };

        let s = self.tt.render(TPL_HTML_NAME, &ctx)?;
        Ok(s)
    }
}
