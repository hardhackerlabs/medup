use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    // Get parameter from command line
    let path = std::env::args().nth(1).expect("not specified path of the markdown file");
    println!("The markdown file: {}", path);

    // Open and read the markdown file
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);

    let mut counter = 0;
    for line in reader.lines() {
        // TODO:
        counter += 1;
        let s = line.unwrap();
        let tokens = parse_tokens(counter, &s);
        println!("{}, {}", counter, s);
    }
}

#[derive(PartialEq, Copy, Clone)]
enum TokenKind {
    Mark,
    Text,
}

struct Token {
    value:      String,
    kind:       TokenKind,
    line_num:   i32,
}

enum State {
    Unknow,
    GuessMark,
    MarkDone,
    ToText,
}

// parse_tokens parses one line text into multi tokens.
fn parse_tokens(ln: i32, line: &String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut state = State::Unknow;
    let mut start: usize = 0;
    let mut end: usize = 0;

    for (i, ch) in line.chars().enumerate() {
        match state {
            State::Unknow => {
                if ch.is_whitespace() {
                    continue
                }
                start = i;
                state = State::GuessMark;
            },
            State::GuessMark => {
                if ch.is_whitespace() {
                    end = i;
                    let sub_str = &line[start..end];
                    // TODO:
                    let (kind, value) = match sub_str {
                        "#" => (TokenKind::Mark, String::from("#")),
                        "##" => (TokenKind::Mark, String::from("##")),
                        "###" => (TokenKind::Mark, String::from("###")),
                        "####" => (TokenKind::Mark, String::from("####")),
                        "#####" => (TokenKind::Mark, String::from("#####")),
                        _ => (TokenKind::Text, String::from("_")),
                    };
                    tokens.push(Token{
                        value:      value,
                        kind:       kind,
                        line_num:   ln,
                    });
                    start = i;
                    end = 0;
                    if kind == TokenKind::Mark {
                        // goto
                        state = State::MarkDone;
                    } else if kind == TokenKind::Text {
                        // goto 
                        state = State::ToText;
                    }
                }
            },
            State::MarkDone => {
                // skip white space
                // TODO:
            },
            State::ToText => (),
        };
    }
    return tokens;
}
