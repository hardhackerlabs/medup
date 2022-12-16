use dice::markdown;

fn main() {
    // Get path for the markdown file from command line arguments
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    // Create Ast object
    let mut ast = markdown::Ast::new();
    ast.parse_file(&path);

    // Output the ast object to help us to check it's correctness
    println!("{:?}", ast);
}
