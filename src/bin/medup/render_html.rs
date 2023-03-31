use std::error::Error;

use serde::Serialize;
use tinytemplate::TinyTemplate;

use crate::config::Config;

// html template
const TP_HTML_NAME: &str = "header";
const TP_HTML: &str = r#"
<head>
<meta charset='UTF-8'><meta name='viewport' content='width=device-width initial-scale=1'>
{{ if css_href }} <link rel="stylesheet" type="text/css" href="{ css_href }"> {{ endif }}
<style>
    body\{
        box-sizing: border-box;
        min-width: {body_min_width}px;
        max-width: {body_max_width}px;
        margin: 0 auto;
        padding: 45px;
    }
</style>
<title>{title}</title>
</head>
<body>
<div {{ if article_class }} class={ article_class }{{ endif }}>
    {{ if use_slice_mode }}
        {{ for s in slices }}
            <div class=slice-div> {s} </div>
        {{ endfor }}
    {{ else }}
        { content }
    {{ endif }}
</div>
</body>
"#;

#[derive(Serialize)]
struct HtmlContext<'html_context> {
    css_href: &'html_context str,
    title: &'html_context str,
    article_class: &'html_context str,
    body_min_width: i32,
    body_max_width: i32,
    use_slice_mode: bool,
    content: &'html_context str,
    slices: &'html_context Vec<String>,
}

pub(crate) struct RenderHtml<'render_html> {
    tt: TinyTemplate<'render_html>,
}

impl<'render_html> RenderHtml<'render_html> {
    pub(crate) fn new() -> Result<Self, Box<dyn Error>> {
        let mut tt = TinyTemplate::new();
        tt.add_template(TP_HTML_NAME, TP_HTML)?;
        tt.set_default_formatter(&tinytemplate::format_unescaped);
        Ok(RenderHtml { tt })
    }

    pub(crate) fn exec(&self, cfg: &Config, data: &Vec<String>) -> Result<String, Box<dyn Error>> {
        let content = if !cfg.use_slice_mode {
            data.join("")
        } else {
            "".to_string()
        };

        let ctx = HtmlContext {
            css_href: &cfg.css_href,
            title: "medup",
            article_class: &cfg.add_class_on_article,
            body_min_width: cfg.body_min_width,
            body_max_width: cfg.body_max_width,
            use_slice_mode: cfg.use_slice_mode,
            content: &content,
            slices: data,
        };

        let s = self.tt.render(TP_HTML_NAME, &ctx)?;
        Ok(s)
    }
}
