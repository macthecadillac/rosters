use embed_manifest::{embed_manifest, new_manifest};

fn main() {
    let manifest_builder = new_manifest("rosters-windows-gui.exe.manifest")
        .dpi_awareness(embed_manifest::manifest::DpiAwareness::System);
    embed_manifest(manifest_builder).expect("unable to embed manifest file");
    println!("cargo:rerun-if-changed=build.rs");
}
