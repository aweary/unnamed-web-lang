use crate::source_map::SourceMap;
use crate::Span;
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use diagnostics::{Diagnostic, Label};

#[derive(Default)]
pub struct ParseSess {
    pub source_map: SourceMap,
}

impl ParseSess {
    pub fn new() -> ParseSess {
        ParseSess {
            source_map: SourceMap::new(),
        }
    }

    /// Error reporting methods for the session
    pub fn fatal(&self, message: &str, label: &str, span: Span) -> Diagnostic {
        let file_id = self
            .source_map
            .current_file
            .expect("Attempted to report error without a current file in the session");
        Diagnostic::new_error(message, Label::new(file_id, span, label))
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(
            &mut writer.lock(),
            &config,
            &self.source_map.files,
            &diagnostic,
        )
        .unwrap();
    }

    /// Emits a warning without buffering, only use if you're absolutely sure that
    /// a warning should be emitted now without any additional context.
    pub fn emit_warning(&self, message: &str, label: &str, span: Span) {
        let file_id = self
            .source_map
            .current_file
            .expect("Attempted to report error without a current file in the session");
        let warning = Diagnostic::new_warning(message, Label::new(file_id, span, label));
        self.emit_diagnostic(warning);
    }

    /// Emits a warning without buffering, only use if you're absolutely sure that
    /// a warning should be emitted now without any additional context.
    pub fn emit_error(&self, message: &str, label: &str, span: Span) {
        let file_id = self
            .source_map
            .current_file
            .expect("Attempted to report error without a current file in the session");
        let error = Diagnostic::new_error(message, Label::new(file_id, span, label));
        self.emit_diagnostic(error);
    }
}
