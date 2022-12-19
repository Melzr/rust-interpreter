fn duplicar(v: &mut i64) {
    *v <<= 1;
}

fn main() {
    let mut w: i64;
    w = 25;
    w <<= 1;
    println!("{}", w);
    duplicar(&mut w);
    println!("{}", w);
}