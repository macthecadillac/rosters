pub fn main() -> std::io::Result<()> {
    ocaml_build::Sigs::new("src/Main.ml").generate()
}
