use med::markdown;

use clap::{arg, Command};

fn main() {
    let matches = cli().get_matches();

    match matches.subcommand() {
        Some(("gen", sub_matches)) => {
            let path = sub_matches
                .get_one::<String>("MARKDOWN_FILE_PATH")
                .expect("required");
            let html = markdown::file_to_html(&path).unwrap();
            println!("{}", html);
        }
        _ => unreachable!(),
    }
}

fn cli() -> Command {
    Command::new("med")
        .about("A markdown parser tool")
        .subcommand_required(true)
        .subcommand(
            Command::new("gen")
                .about("generate HTML based on markdown")
                .arg(arg!(<MARKDOWN_FILE_PATH>)),
        )
}
