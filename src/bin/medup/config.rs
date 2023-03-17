use std::{error::Error, fs::File, io::Read};

use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Config {
    pub css_href: String,
    pub add_class_on_article: String,
}

impl Config {
    pub(crate) fn read(path: &str) -> Result<Config, Box<dyn Error>> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;
        let cfg: Config = serde_json::from_str(&buf)?;
        Ok(cfg)
    }
}
