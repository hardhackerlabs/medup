use std::{error::Error, fs::File, io::Read, path::Path};

use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub(crate) struct ConfigJson {
    pub body_min_width: i32,
    pub body_max_width: i32,
    pub use_slice_mode: bool,
    pub slice_header: String,
}

#[derive(Default, Debug, Clone)]
pub(crate) struct Config {
    pub config_json: ConfigJson,
    pub template: String,
}

impl Config {
    pub(crate) fn read(path: &str) -> Result<Config, Box<dyn Error>> {
        let mut buf = String::new();
        File::open(path)?.read_to_string(&mut buf)?;

        let mut cjson: ConfigJson = serde_json::from_str(&buf)?;
        if cjson.body_min_width == 0 {
            cjson.body_min_width = 200;
        }
        if cjson.body_max_width == 0 {
            cjson.body_max_width = 900;
        }

        let mut cfg: Config = Config::default();
        cfg.config_json = cjson;

        // read template
        Path::new(path).parent().map(|p| {
            let mut template_path = p.to_path_buf();
            template_path.push("template.txt");
            let mut buf = String::new();
            File::open(template_path)
                .map(|mut f| f.read_to_string(&mut buf))
                .ok();
            cfg.template = buf;
        });

        Ok(cfg)
    }
}
