mod gen;
mod serve;

use crate::gen::proc_gen;
use crate::serve::proc_serve;

use clap::{arg, Command};

fn main() {
    let matches = cli().get_matches();
    match matches.subcommand() {
        Some(("gen", sub_matches)) => proc_gen(sub_matches),
        Some(("serve", sub_matches)) => proc_serve(sub_matches),
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
                .about("generate HTML based on Markdown!")
                .arg(arg!(-c --"config-path" [CONFIG_PATH] "Specify path of the config file, it's optional."))
                .arg(arg!(-o --output [OUTPUT_HTML_PATH] "Specify a html output path, it's optional."))
                .arg(arg!(<MARKDOWN_FILE_PATH>)),
        )
        .subcommand(
            Command::new("serve")
                .about("Provide an http service for markdown parsing!")
                .arg(arg!(-l --"listen-addr" [LISTEN_ADDR] r#"Specify the listening address of the http server, default ":8181"."#))
                .arg(arg!(-c --"config-path" [CONFIG_PATH] "Specify path of the config file, it's optional."))
                .arg(arg!(-d --dir [DIR] "Specify the directory where markdown files are stored."))
                .arg(arg!(-s --"static-dir" [STATIC_DIR] "Specify the directory where static resources are stored."))
        )
}
