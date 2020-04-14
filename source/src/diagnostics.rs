use crate::filesystem::{FileId, FileSystem, Files};
pub use codespan::{ByteIndex, Span};
use codespan_reporting::diagnostic::{
    Diagnostic as CodespanDiagnostic, Label as CodespanLabel, LabelStyle, Severity,
};
pub use codespan_reporting::term::*;
use lsp_types::{
    Diagnostic as LspDiagnostic, DiagnosticSeverity, Position, Range as LspRange, Url,
};
use std::ops::Range;

// These diagnostic interfaces implement the same API as the codespan_reporting crate, except
// Label, which does not require a FileId when instantiated.

#[derive(Default)]
pub struct DiagnosticSet {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticSet {
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic)
    }
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub labels: Vec<Label>,
    severity: Severity,
}

impl Diagnostic {
    pub fn into_lsp(self, files: &FileSystem) -> (LspDiagnostic, Url) {
        // Find the first primary label
        let primary_label: &Label = self
            .labels
            .iter()
            .find(|label| label.style == LabelStyle::Primary)
            .expect("Diagnostics must have a primary label");
        let file = primary_label.file.expect("Labels must have files");
        // Get URI for the diagnostic from the file path
        let path = files.path_for_id(&file);
        let url = Url::from_file_path(path).unwrap();
        let range = {
            let range = &primary_label.range;
            // codespan-reporting Locations
            let start = files.location(file, range.start).unwrap();
            let end = files.location(file, range.end).unwrap();

            // lsp-types Positions
            let start = Position::new(
                (start.line_number - 1) as u64,
                (start.column_number - 1) as u64,
            );
            let end = Position::new((end.line_number - 1) as u64, (end.column_number - 1) as u64);
            LspRange::new(start, end)
        };
        let severity = match self.severity {
            Severity::Error | Severity::Bug => DiagnosticSeverity::Error,
            Severity::Warning => DiagnosticSeverity::Warning,
            Severity::Note => DiagnosticSeverity::Information,
            Severity::Help => DiagnosticSeverity::Hint,
        };
        let related_information = None;
        let tags = None;
        let code = None;
        let source: String = files.source(file).unwrap();
        let mut message = self.message.clone();
        message.push_str(": ");
        message.push_str(&primary_label.message);
        let diagnostic = LspDiagnostic::new(
            range,
            Some(severity),
            code,
            Some(source),
            message,
            related_information,
            tags,
        );
        (diagnostic, url)
    }

    pub fn emit_to_terminal(self, files: &FileSystem) {
        use termcolor::{ColorChoice, StandardStream};
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = Config::default();
        // Convert to a codespan-reporting diagnostic
        let diagnostic = CodespanDiagnostic::new(self.severity)
            .with_message(self.message)
            .with_labels(
                self.labels
                    .into_iter()
                    .map(|label| {
                        let file = label.file.expect("Must have a file when reporting a label");
                        CodespanLabel::new(label.style, file, label.range)
                            .with_message(label.message)
                    })
                    .collect(),
            );
        emit(&mut writer.lock(), &config, &*files, &diagnostic).expect("Emitting");
    }

    pub fn error() -> Diagnostic {
        Diagnostic {
            message: String::new(),
            labels: vec![],
            severity: Severity::Error,
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
