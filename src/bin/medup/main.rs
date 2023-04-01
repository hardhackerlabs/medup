mod config;
mod render_html;
mod serve;

use crate::serve::proc_serve;

use clap::{arg, Command};

fn main() {
    let matches = cli().get_matches();
    match matches.subcommand() {
        Some(("serve", sub_matches)) => proc_serve(sub_matches),
        _ => unreachable!(),
    }
}

fn cli() -> Command {
    Command::new("medup")
        .version("0.1")
        .about("A markdown parser and reader")
        .subcommand_required(true)
        .subcommand(
            Command::new("serve")
                .about("Provide an http service for markdown parsing")
                .arg(arg!(-l --"listen-addr" [LISTEN_ADDR] r#"Specify the listening address of the http server, default ":8181"."#))
                .arg(arg!(-c --"config-path" [CONFIG_PATH] "Specify path of the config file, it's optional."))
                .arg(arg!(-d --dir [DIR] "Specify the directory where markdown files are stored."))
                .arg(arg!(-s --"static-dir" [STATIC_DIR] "Specify the directory where static resources are stored."))
        )
}
