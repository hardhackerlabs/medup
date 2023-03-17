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
                min-width: 200px;
                max-width: 900px;
                margin: 0 auto;
                padding: 45px;
        }

        @media (max-width: 767px) \{
                body\{
                        padding: 15px;
                }
        }
</style>
<title>{title}</title>
</head>
<body>
<article{{ if article_class }} class={ article_class }{{ endif }}>
    { content }
</article>
</body>
"#;

#[derive(Serialize)]
struct HtmlContext<'html_context> {
    css_href: &'html_context str,
    title: &'html_context str,
    article_class: &'html_context str,
    content: &'html_context str,
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

    pub(crate) fn exec(&self, cfg: &Config, content: &str) -> Result<String, Box<dyn Error>> {
        let ctx = HtmlContext {
            css_href: &cfg.css_href,
            title: "medup",
            article_class: &cfg.add_class_on_article,
            content,
        };
        let s = self.tt.render(TP_HTML_NAME, &ctx)?;
        Ok(s)
    }
}
