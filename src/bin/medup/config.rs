use std::{error::Error, fs::File, io::Read};

use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub(crate) struct Config {
    pub css_href: String,
    pub add_class_on_article: String,
    pub body_min_width: i32,
    pub body_max_width: i32,
    pub use_slice_mode: bool,
    pub slice_header: String,
}

impl Config {
    pub(crate) fn read(path: &str) -> Result<Config, Box<dyn Error>> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;

        let mut cfg: Config = serde_json::from_str(&buf)?;
        if cfg.body_min_width == 0 {
            cfg.body_min_width = 200;
        }
        if cfg.body_max_width == 0 {
            cfg.body_max_width = 900;
        }

        Ok(cfg)
    }
}
