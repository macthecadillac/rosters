pub fn main() -> std::io::Result<()> {
    ocaml_build::Sigs::new("src/rust_bindgen.ml").generate()
}
