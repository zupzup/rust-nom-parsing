use rust_nom_parsing::uri;

fn main() {
    let res = uri("https://zupzup.org/about");
    println!("Result: {:?}", res);

    let res = uri("https:/invalid");
    println!("Result: {:?}", res);
}
