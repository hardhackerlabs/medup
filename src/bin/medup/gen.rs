use std::fs::File;
use std::io::Write;

use medup::config::{self, Config};
use medup::markdown::{self, Markdown};

use clap::ArgMatches;

pub fn proc_gen(sub_matches: &ArgMatches) {
    // read config path from cli
    let cfg = match sub_matches.get_one::<String>("config-path") {
        None => Config::default(),
        Some(path) => config::read_config(path)
            .map_err(|e| format!("failed to read config \"{}\": {}", path, e))
            .unwrap(),
    };

    // read output file path from cli
    let out_file = match sub_matches.get_one::<String>("output") {
        None => None,
        Some(path) => File::options()
            .write(true)
            .truncate(true)
            .create(true)
            .open(path)
            .map(Some)
            .unwrap(),
    };

    // read markdown file path from cli
    let md_path = sub_matches
        .get_one::<String>("MARKDOWN_FILE_PATH")
        .expect("required");

    // start to parse the markdown file into html
    let html = Markdown::new()
        .config(cfg)
        .path(md_path)
        .map_mut(markdown::to_html)
        .unwrap();

    // output the html
    if let Some(mut out) = out_file {
        Write::write_all(&mut out, html.as_bytes()).unwrap();
    } else {
        println!("{}", html);
    }
}
