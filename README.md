# Medup is a markdown parser and reader!

![](./docs/assets/logo.png)


## Features
* [x] Support all standard syntax of markdown
* [x] Supports all major extension syntaxes
* [x] Provide **Crate** to convert markdown to html or Custom development based on AST directly
* [x] Provide **CLI** tool to convert markdown to html
* [x] Provide **Web Service** to host and parse markdown files, and create private document system or blog
* [x] Support css theme selection
* [ ] Custom new grammar so that the content can be layout horizontally
* [ ] Support git as storage backend
* [ ] There may be an editor, supporting VIM mode

## Demo

![](./docs/assets/demo.png)

## Usage
```
A markdown parser and reader

Usage: medup <COMMAND>

Commands:
  gen    Generate html from markdown
  serve  Provide an http service for markdown parsing
  help   Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help information
  -V, --version  Print version information
```

### CLI

Use the following command to generate a html file from your markdown file.
```
cargo run -- gen --output markdown-guide.html docs/markdown-guide.md
```

### Web

Use the following command to start an http service on port 8181.
```
cargo run -- serve --config-path themes/notion/config.json --dir docs --static-dir themes
```
or

```
docker run -d --rm -p 8181:8181 skoowoo/medup:0.1
```

Open `http://localhost:8181` with your browser.

### Crate

```Rust
// Cargo.toml
// medup = {git = "https://github.com/skoowoo/medup"}

use medup::{config, markdown};

let html_content = markdown::Markdown::new()
    .config(config::Config::default())
    .path("docs/markdown-guide.md")
    .map_mut(markdown::to_html_body);

println!(html_content);
```
