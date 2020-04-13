use crate::filesystem::FileId;
pub use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{
    Diagnostic as CodespanDiagnostic, Label as CodespanLabel, LabelStyle, Severity,
};
pub use codespan_reporting::term::*;
use std::ops::Range;



// These diagnostic interfaces implement the same API as the codespan_reporting crate, except
// Labels

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub labels: Vec<Label>,
}

impl Diagnostic {
    pub fn error() -> Diagnostic {
        Diagnostic {
            message: String::new(),
            labels: vec![],
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    pub fn with_labels(mut self, labels: impl Into<Vec<Label>>) -> Self {
        self.labels = labels.into();
        self
    }

    pub fn for_file(mut self, file: FileId) -> Self {
        for mut label in &mut self.labels {
            if label.file.is_none() {
                label.file = Some(file);
            }
        }
        self
    }
}

#[derive(Clone, Debug)]
pub struct Label {
    pub file: Option<FileId>,
    pub style: LabelStyle,
    pub range: Range<usize>,
    pub message: String,
}

impl Label {
    pub fn primary(range: impl Into<Range<usize>>) -> Label {
        Label {
            file: None,
            style: LabelStyle::Primary,
            range: range.into(),
            message: String::new(),
        }
    }

    pub fn secondary(range: impl Into<Range<usize>>) -> Label {
        Label {
            file: None,
            style: LabelStyle::Secondary,
            range: range.into(),
            message: String::new(),
        }
    }

    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        self.message = message.into();
        self
    }

    pub fn for_file(mut self, file: FileId) -> Self {
        self.file = Some(file);
        self
    }

}
