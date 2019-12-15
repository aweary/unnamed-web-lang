use crate::source_map::SourceMap;
use crate::Span;
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use diagnostics::{Diagnostic, Label};

use std::fs;
use std::path::PathBuf;

#[derive(Default)]
pub struct ParseSess {
    pub source_map: SourceMap,
}

impl ParseSess {
    /// Load a new file into the parsing session.
    pub fn load_file(&mut self, path: PathBuf) {
        // Read the file into memory
        let source = fs::read_to_string(path).expect("File not found");
    }

    /// Error reporting methods for the session
    pub fn fatal(&self, message: &str, label: &str, span: Span) -> Diagnostic {
        println!("fatal {:?}", message);
        let file_id = self.source_map.current_file_id();
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
        println!("emit warning {:?}", message);
        let file_id = self.source_map.current_file_id();
        let warning = Diagnostic::new_warning(message, Label::new(file_id, span, label));
        self.emit_diagnostic(warning);
    }

    /// Emits a warning without buffering, only use if you're absolutely sure that
    /// a warning should be emitted now without any additional context.
    pub fn emit_error(&self, message: &str, label: &str, span: Span) {
        println!("emit error {:?}", message);
        let file_id = self.source_map.current_file_id();
        let error = Diagnostic::new_error(message, Label::new(file_id, span, label));
        self.emit_diagnostic(error);
    }
}
