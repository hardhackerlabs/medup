use std::{error::Error, fs::File, io::Read};

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub css_href: String,
    pub custom_html_header: String,
    pub add_class_on_body: String,
    pub add_class_on_article: String,
}

impl Config {
    pub fn default() -> Self {
        Config {
            css_href: String::from(""),
            custom_html_header: String::from(""),
            add_class_on_article: String::from(""),
            add_class_on_body: String::from(""),
        }
    }
}

pub fn read_config(path: &str) -> Result<Config, Box<dyn Error>> {
    let mut buf = String::new();
    File::open(path)?.read_to_string(&mut buf)?;
    let cfg: Config = serde_json::from_str(&buf)?;
    Ok(cfg)
}
