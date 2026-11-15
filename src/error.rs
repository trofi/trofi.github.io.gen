use thiserror::Error;

#[derive(Error, Debug)]
pub(crate) enum BlogError {
    #[error("Logic error: {0}")]
    Logic(&'static str),

    #[error("Command failed")]
    CommandFailed,

    #[error("IO error: {0}")]
    IO(std::io::Error),

    #[error("MacroParse error: {0:?}")]
    MacroParse(math_core::MacroParseError),

    #[error("LatexParse error: {0:?}")]
    LatexParse(Box<math_core::LatexError>),

    #[error("Fmt error: {0:?}")]
    Fmt(std::fmt::Error),

    #[error("Glob error: {0:?}")]
    Glob(glob::GlobError),

    #[error("Pattern error: {0:?}")]
    Pattern(glob::PatternError),

    #[error("Url parse error: {0:?}")]
    UrlParse(url::ParseError),

    #[error("From Utf8 error: {0:?}")]
    FromUtf8(std::string::FromUtf8Error),

    #[error("HTML Rewriting error: {0:?}")]
    HTMLRewriting(lol_html::errors::RewritingError),
}

impl From<std::fmt::Error> for BlogError {
    fn from(error: std::fmt::Error) -> Self {
        BlogError::Fmt(error)
    }
}

impl From<std::io::Error> for BlogError {
    fn from(error: std::io::Error) -> Self {
        BlogError::IO(error)
    }
}

impl From<math_core::MacroParseError> for BlogError {
    fn from(error: math_core::MacroParseError) -> Self {
        BlogError::MacroParse(error)
    }
}

impl From<Box<math_core::LatexError>> for BlogError {
    fn from(error: Box<math_core::LatexError>) -> Self {
        BlogError::LatexParse(error)
    }
}

impl From<glob::PatternError> for BlogError {
    fn from(error: glob::PatternError) -> Self {
        BlogError::Pattern(error)
    }
}

impl From<glob::GlobError> for BlogError {
    fn from(error: glob::GlobError) -> Self {
        BlogError::Glob(error)
    }
}

impl From<url::ParseError> for BlogError {
    fn from(error: url::ParseError) -> Self {
        BlogError::UrlParse(error)
    }
}

impl From<std::string::FromUtf8Error> for BlogError {
    fn from(error: std::string::FromUtf8Error) -> Self {
        BlogError::FromUtf8(error)
    }
}

impl From<lol_html::errors::RewritingError> for BlogError {
    fn from(error: lol_html::errors::RewritingError) -> Self {
        BlogError::HTMLRewriting(error)
    }
}
