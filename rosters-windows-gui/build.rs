use embed_manifest::{embed_manifest, new_manifest};

fn main() {
    embed_manifest(new_manifest("rosters-windows-gui.exe.manifest"))
        .expect("unable to embed manifest file");
    println!("cargo:rerun-if-changed=build.rs");
}
