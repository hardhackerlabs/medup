use med::markdown;

fn main() {
    // Get path for the markdown file from command line arguments
    let path = std::env::args()
        .nth(1)
        .expect("not specified path of the markdown file");

    let html = markdown::file_to_html(&path).unwrap();
    println!("{}", html);
}
