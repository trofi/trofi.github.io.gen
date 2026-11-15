pub use clap::Parser;

/// Build static blog site.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub(crate) struct Opts {
    /// Enable extra verbosity to report unexpected events,
    /// build progress and so on.
    #[command(flatten)]
    pub(crate) verbose: clap_verbosity_flag::Verbosity<clap_verbosity_flag::InfoLevel>,

    /// Pass output directory to store output to.
    #[arg(short, long, default_value = "out")]
    pub(crate) out: String,
}
