use clap::{Parser, Subcommand};
use main_error::MainError;

use std::fs;
use std::path::PathBuf;

use rosters_lib::{EXAMPLE_CONFIG, Generator, Lab};

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
        #[arg(short, long)]
        ngroups: Option<usize>,
        #[arg(long="group-size")]
        group_size: Option<usize>,
        /// Lab number
        #[arg(long)]
        lab: Lab,
        /// Omit the 'Signed' column
        #[arg(long)]
        no_sign: bool,
        /// Skip Excel file generation
        #[arg(long)]
        nox: bool,
        /// Run with supplied configuration
        #[arg(short='c', long="config", alias="with-config")]
        config: Option<PathBuf>,
        /// Run with default configuration. This flag overrides the `--config` option
        #[arg(long="defaults")]
        defaults: bool,
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
        Subcmd::Generate { input, output, ngroups, group_size, lab, no_sign,
                           nox, config, defaults, no_split } => {
            let generator = Generator { input, output, lab, ngroups, group_size,
                                        no_sign, nox, config, defaults, no_split };
            generator.run()?
        }
    }
    Ok(())
}
