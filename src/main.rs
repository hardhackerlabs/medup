use dice::markdown;
use std::fs::File;
use std::io;

fn main() {
    // Get path for the markdown file from command line arguments
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    // Open and read the markdown file by line
    let file = File::open(path).unwrap();
    let reader = io::BufReader::new(file);

    // Create Ast object
    let mut ast = markdown::Ast::new();
    ast.parse_file(reader);

    // Output the ast object to help us to check it's correctness
    println!("{:?}", ast);
}
