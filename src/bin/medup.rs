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

mod gen {
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
                .map(|f| Some(f))
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
}

mod serve {
    use std::error::Error;
    use std::net::Ipv4Addr;
    use std::path::Path;

    use medup::config::{self, Config};
    use medup::markdown::{self, Markdown};

    use clap::ArgMatches;
    use warp::filters::BoxedFilter;
    use warp::{http::Response, Filter, Reply};

    #[tokio::main]
    pub async fn proc_serve(matches: &ArgMatches) {
        let (addr, port) = parse_ip_port(matches).unwrap();
        let cfg: Config = load_config(matches).unwrap();
        let dir = get_dir(matches, "dir");
        let sdir = get_dir(matches, "static-dir");

        println!(
            "---> the directory where markdown files are stored: \"{}\"",
            dir
        );
        println!(
            "---> the directory where static resources are stored: \"{}\"",
            sdir
        );

        // All filters are used to match requests
        let filters = articles_filter(cfg, dir.to_string())
            .or(static_filter(sdir.to_string()))
            .or(index_filter());

        println!("---> start to listen on address: \"{}:{}\"", addr, port);
        warp::serve(filters).run((addr, port)).await
    }

    // TODO:
    fn index_filter() -> BoxedFilter<(impl Reply,)> {
        warp::get()
            .and(warp::path("index.html"))
            .map(|| Response::builder().body("Hello, Medup!").into_response())
            .with(warp::cors().allow_any_origin())
            .boxed()
    }

    fn static_filter(dir: String) -> BoxedFilter<(impl Reply,)> {
        warp::get()
            .and(warp::path("static"))
            .and(warp::fs::dir(dir))
            .with(warp::cors().allow_any_origin())
            .boxed()
    }

    // Get /articles/:name (/articles/demo.md)
    fn articles_filter(cfg: Config, dir: String) -> BoxedFilter<(impl Reply,)> {
        warp::get()
            .and(warp::path("articles"))
            .and(warp::path::param::<String>())
            .and(warp::any().map(move || cfg.clone()))
            .and(warp::any().map(move || dir.to_string()))
            .map(|name: String, cfg: Config, dir: String| {
                let buf = Path::new(&dir).join(&name);
                let s = buf.to_str();
                match s {
                    None => Response::builder()
                        .header("X-Powered-By", "Medup")
                        .status(400)
                        .body(format!(
                            "failed to join the path: \"{}\", \"{}\"",
                            dir, name
                        ))
                        .into_response(),
                    Some(path) => match Markdown::new()
                        .config(cfg)
                        .path(path)
                        .map_mut(markdown::to_html)
                    {
                        Err(e) => Response::builder()
                            .header("X-Powered-By", "Medup")
                            .status(500)
                            .body(format!("failed to generate html from markdown: {}", e))
                            .into_response(),
                        Ok(v) => warp::reply::html(v).into_response(),
                    },
                }
            })
            .with(warp::cors().allow_any_origin())
            .boxed()
    }

    fn get_dir<'get_dir>(matches: &'get_dir ArgMatches, name: &str) -> &'get_dir str {
        match matches.get_one::<String>(name) {
            None => ".",
            Some(path) => path,
        }
    }

    fn load_config(matches: &ArgMatches) -> Result<Config, Box<dyn Error>> {
        // read config path from cli
        let mut cfg = match matches.get_one::<String>("config-path") {
            None => Config::default(),
            Some(path) => config::read_config(path)
                .map_err(|e| (format!("failed to read config \"{}\": {}", path, e)))?,
        };
        if !medup::is_url(&cfg.css_href) {
            // add the static resource dir to css_href
            if let Some(href) = Path::new("/static")
                .join(&cfg.css_href)
                .to_str()
                .map(|s| s.to_string())
            {
                cfg.css_href = href;
            }
        }
        Ok(cfg)
    }

    fn parse_ip_port(matches: &ArgMatches) -> Result<(Ipv4Addr, u16), Box<dyn Error>> {
        // read listen addr from the args of command line
        let s = match matches.get_one::<String>("listen-addr") {
            None => ":8181",
            Some(s) => s,
        };

        let fields: Vec<&str> = s.split(':').collect();
        if fields.len() != 2 {
            return Err(format!("invalid address format: {}", s).into());
        }
        let ipaddr: Ipv4Addr = if fields[0].is_empty() {
            "0.0.0.0"
                .parse()
                .map_err(|e| format!("failed to parse ip addr 0.0.0.0: {}", e))?
        } else {
            fields[0]
                .parse()
                .map_err(|e| format!(r#"failed to parse ip addr "{}": {}"#, fields[0], e))?
        };
        let port: u16 = fields[1]
            .parse()
            .map_err(|e| format!("failed to parse port: {}: {}", fields[1], e))?;

        Ok((ipaddr, port))
    }
}
