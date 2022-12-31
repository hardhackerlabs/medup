use med::render;

fn main() {
    // Get path for the markdown file from command line arguments
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    let html = render::process_file(&path).unwrap();
    println!("{}", html);
}
