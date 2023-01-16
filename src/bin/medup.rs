use std::fs::File;
use std::io::Write;

use medup::config::{self, Config};
use medup::markdown;

use clap::{arg, Command};

fn main() {
    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("gen", sub_matches)) => {
            // read config path from cli
            let cfg = match sub_matches.get_one::<String>("config-path") {
                None => Config::default(),
                Some(path) => config::read_config(path).unwrap(),
            };

            // read output file path from cli
            let out_file = match sub_matches.get_one::<String>("output") {
                None => None,
                Some(path) => File::options()
                    .write(true)
                    .truncate(true)
                    .create(true)
                    .open(path)
                    .map(|f| Some(f))
                    .unwrap(),
            };

            // read markdown file path from cli
            let md_path = sub_matches
                .get_one::<String>("MARKDOWN_FILE_PATH")
                .expect("required");

            // start to parse the markdown file into html
            let html = markdown::file_to_html(&cfg, md_path).unwrap();

            // output the html
            if let Some(mut out) = out_file {
                // out.write(html.as_bytes()).unwrap();
                Write::write_all(&mut out, html.as_bytes()).unwrap();
            } else {
                println!("{}", html);
            }
        }
        _ => unreachable!(),
    }
}

fn cli() -> Command {
    Command::new("medup")
        .version("0.1")
        .about("A markdown parsing toolkit")
        .subcommand_required(true)
        .subcommand(
            Command::new("gen")
                .about("generate HTML based on Markdown")
                .arg(arg!(-c --"config-path" [CONFIG_PATH] "Specify path of the config file, it's optional."))
                .arg(arg!(-o --output [OUTPUT_HTML_PATH] "Specify a html output path, it's optional."))
                .arg(arg!(<MARKDOWN_FILE_PATH>)),
        )
}
