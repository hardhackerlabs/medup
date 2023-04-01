use std::{error::Error, fs::File, io::Read, path::Path};

use serde::{Deserialize, Serialize};

const TEMPLATE_FILE_NAME: &str = "template.txt";

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub(crate) struct ConfigJson {
    pub body_min_width: i32,
    pub body_max_width: i32,
    pub use_slice_mode: bool,
    pub slice_header: String,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct Config {
    config_json: ConfigJson,
    template: String,
}

impl Config {
    pub(crate) fn body_min_width(&self) -> i32 {
        self.config_json.body_min_width
    }

    pub(crate) fn body_max_width(&self) -> i32 {
        self.config_json.body_max_width
    }

    pub(crate) fn use_slice_mode(&self) -> bool {
        self.config_json.use_slice_mode
    }

    pub(crate) fn slice_header(&self) -> &str {
        &self.config_json.slice_header
    }

    pub(crate) fn template(&self) -> &str {
        &self.template
    }

    pub(crate) fn read(path: &str) -> Result<Config, Box<dyn Error>> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;

        let mut cj: ConfigJson = serde_json::from_str(&buf)?;
        if cj.body_min_width == 0 {
            cj.body_min_width = 200;
        }
        if cj.body_max_width == 0 {
            cj.body_max_width = 900;
        }

        // read template
        let mut buf = String::new();
        Path::new(path).parent().map(|p| {
            let mut template_path = p.to_path_buf();
            template_path.push(TEMPLATE_FILE_NAME);
            File::open(template_path)
                .map(|mut f| f.read_to_string(&mut buf))
                .ok();
        });

        Ok(Config {
            config_json: cj,
            template: buf,
        })
    }
}
