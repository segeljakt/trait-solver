// use clap::Parser;
// use std::ffi::OsString;
// use std::path::PathBuf;
//
// fn history() -> OsString {
//     std::env::temp_dir()
//         .join("aqua")
//         .join("history.txt")
//         .into_os_string()
// }
//
// #[derive(Debug, Default, Clone, Parser)]
// pub struct Config {
//     /// Read source from file
//     pub file: Option<PathBuf>,
//     /// Loads file statement-by-statement into the REPL.
//     #[clap(long)]
//     pub interactive: bool,
//     /// Print version
//     #[clap(long)]
//     pub version: bool,
//     #[clap(long, default_value = history())]
//     pub history: PathBuf,
// }
//
// mod build {
//     include!(concat!(env!("OUT_DIR"), "/built.rs"));
// }
//
// pub fn version() {
//     println!(" version: {}", build::PKG_VERSION);
//     println!("  target: {}", build::TARGET);
//     println!(" profile: {}", build::PROFILE);
//     println!("compiler: {}", build::RUSTC_VERSION);
//     println!("    time: {}", build::BUILT_TIME_UTC);
// }

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let config = Config::parse();
    // if config.version {
    //     version();
    //     return Ok(());
    // }
    // Ok(())
    Ok(())
}
