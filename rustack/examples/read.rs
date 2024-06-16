
fn main() {
    for line in std::io::stdin().lines() {
        if let Ok(line) = line {
            let words : Vec<_> = line.split(" ").collect();
            println!("Line: {words:?}")
        }

    }
}