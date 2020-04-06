pub use codespan_reporting::diagnostic::{Diagnostic, LabelStyle, Label};
pub use codespan_reporting::term::*;
pub use codespan::{Span, ByteIndex};
use crate::filesystem::FileId;



pub struct Diagnostic_ {
    message: String,
    labels: Vec<Label_>
}

pub struct Label_ {

}

pub struct DiagnosticsBuilder {
    diagnostics: Vec<Diagnostic<FileId>>,
}


