use source::diagnostics::Diagnostic;
use source::filesystem::FileId;

/// Both the lexer and parser return a Diagnostic for errors.
/// This might also be extended to other phases like typechecking
/// in the future.
pub type ParseResult<T> = std::result::Result<T, Diagnostic<FileId>>;
