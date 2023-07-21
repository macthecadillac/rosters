use clap::{Parser, Subcommand};
use main_error::MainError;

use std::fs;
use std::path::PathBuf;

use rosters_lib::*;

#[derive(Debug, Subcommand)]
enum Subcmd {
    /// Create starter configuration
    Config {
        /// Output path
        #[arg(short, long)]
        output: std::path::PathBuf
    },
    /// Generate rosters
    Generate {
        /// Path to canvas exported CSV file
        #[arg(short, long)]
        input: PathBuf,
        /// Output directory
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Lab number
        #[arg(long)]
        lab: usize,
        /// Skip Excel file generation
        #[arg(long)]
        nox: bool,
        /// Run with supplied configuration
        #[arg(short='c', long="with-config")]
        config: Option<PathBuf>,
        /// Do not create per-section or per-TA PDFs
        #[arg(long="no-split")]
        no_split: bool
    },
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[command(subcommand)]
    command: Subcmd,
}

fn main() -> Result<(), MainError> {
    let args = Args::parse();
    match args.command {
        Subcmd::Config { output } => fs::write(&output, crate::EXAMPLE_CONFIG)?,
        Subcmd::Generate { input, output, lab, nox, config, no_split } => 
            crate::generate(input, output, lab, nox, config, no_split)?
    }
    Ok(())
}
