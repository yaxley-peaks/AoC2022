fn has_dups(xs: &[u8]) -> bool {
    !(xs.iter().collect::<std::collections::HashSet<_>>().len() == xs.len())
}
fn main() {
    println!("{} {}", 
        std::fs::read("inp.txt").unwrap().windows(4).take_while(|x|has_dups(x)).count() + 4
    ,   std::fs::read("inp.txt").unwrap().windows(14).take_while(|x|has_dups(x)).count() + 14
    );
}