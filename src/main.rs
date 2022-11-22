use std::fs::File;
use std::io::{self, BufRead, Error};

fn main() -> Result<(), Error> {
    // Get parameter from command line
    let path = std::env::args().nth(1).expect("not specified path of the markdown file");
    println!("The markdown file: {}", path);

    // Open and read the markdown file
    let file = File::open(path)?;
    let reader = io::BufReader::new(file);
    for line in reader.lines() {
        println!("{}", line?);
    }

    Ok(())
}
