use std::fs::File;
use std::io::Write;

use crate::{config::Config, render_html::RenderHtml};
use medup::markdown::{self, Markdown};

use clap::ArgMatches;

pub fn proc_gen(sub_matches: &ArgMatches) {
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
    let v = Markdown::new()
        .path(md_path)
        .map_mut(markdown::to_body)
        .unwrap();
    let render: RenderHtml = RenderHtml::new().expect("failed to add html template");
    let html = render.exec(&Config::default(), &v).unwrap();

    // output the html
    if let Some(mut out) = out_file {
        Write::write_all(&mut out, html.as_bytes()).unwrap();
    } else {
        println!("{}", html);
    }
}
