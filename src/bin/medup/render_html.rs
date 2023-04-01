use std::error::Error;

use serde::Serialize;
use tinytemplate::TinyTemplate;

use crate::config::Config;

// html template
const TP_HTML_NAME: &str = "template";

#[derive(Serialize)]
struct HtmlContext<'html_context> {
    title: &'html_context str,
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
        tt.add_template(TP_HTML_NAME, template)?;
        tt.set_default_formatter(&tinytemplate::format_unescaped);
        Ok(RenderHtml { tt })
    }

    pub(crate) fn exec(&self, cfg: &Config, data: &Vec<String>) -> Result<String, Box<dyn Error>> {
        let content = if !cfg.config_json.use_slice_mode {
            data.join("")
        } else {
            "".to_string()
        };

        let ctx = HtmlContext {
            title: "medup",
            body_min_width: cfg.config_json.body_min_width,
            body_max_width: cfg.config_json.body_max_width,
            use_slice_mode: cfg.config_json.use_slice_mode,
            slice_header: &cfg.config_json.slice_header,
            content: &content,
            slices: data,
        };

        let s = self.tt.render(TP_HTML_NAME, &ctx)?;
        Ok(s)
    }
}
