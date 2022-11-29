use mn::Ast;
use mn::Line;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    // Create Ast object
    let mut ast = crate::Ast::new();

    // Get path for the markdown file from command line arguments
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    // Open and read the markdown file by line
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);

    let mut counter = 0;
    for text in reader.lines() {
        counter += 1;
        let mut s = text.unwrap();
        s.push('\n');
        let line = Line::parse(counter, s);
        ast.push(line);
    }

    // Output the ast object to help us to check it's correctness
    println!("{:?}", ast);
}
