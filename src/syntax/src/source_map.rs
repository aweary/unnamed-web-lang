use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use codespan::{FileId, Files, Span};
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use diagnostics::{Diagnostic, Label};

pub struct SourceMap {
    pub files: Files,
    pub current_file: Option<FileId>,
    loader: FileLoader,
}

impl Default for SourceMap {
    fn default() -> Self {
        SourceMap {
            files: Files::new(),
            current_file: None,
            loader: Default::default(),
        }
    }
}

impl SourceMap {
    pub fn new() -> SourceMap {
        SourceMap {
            files: Files::new(),
            current_file: None,
            loader: Default::default(),
        }
    }

    pub fn load_file(&mut self, path_str: &str) -> FileId {
        let path = Path::new(path_str);
        let source = self.loader.read_file(path).unwrap();
        let id = self.files.add(path_str, source);
        self.current_file = Some(id);
        id
    }

    pub fn report_error(&self, message: &str, span: Span, label: &str) {
        let id = self.current_file.expect("No file loaded.");
        let err = Diagnostic::new_error(message, Label::new(id, span, label));
        // TODO we should buffer diagnostics and report all at once
        // at the end of parsing
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(&mut writer.lock(), &config, &self.files, &err).unwrap();
    }
}

#[derive(Clone)]
pub struct SourceFile {
    path: PathBuf,
}

#[derive(Default)]
pub struct FileLoader {}

impl FileLoader {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }

    fn _to_absolute(&self, path: &Path) -> Option<PathBuf> {
        if path.is_absolute() {
            Some(path.to_path_buf())
        } else {
            env::current_dir().ok().map(|cwd| cwd.join(path))
        }
    }
}
