use source::diagnostics::Diagnostic;

pub type ParseResult<T> = std::result::Result<T, Diagnostic>;
