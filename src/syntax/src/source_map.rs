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
    pub current_file: Option<SourceFile>,
    loader: FileLoader,
}

impl Default for SourceMap {
    fn default() -> Self {
        SourceMap {
            files: Files::new(),
            current_file: Default::default(),
            loader: Default::default(),
        }
    }
}

impl SourceMap {
    pub fn load_file_from_path(&mut self, path: &Path) -> FileId {
        let abs_path = self
            .loader
            .to_absolute(&path)
            .expect("Failed to convert to absolute path");
        println!("loading path {:?}", abs_path);
        let source = self.loader.read_file(&path).unwrap();
        let id = self.files.add(path.to_str().unwrap(), source);
        let source_file = SourceFile {
            path: path.to_path_buf(),
            abs_path,
            id,
        };
        self.current_file = Some(source_file);
        id
    }

    pub fn load_file(&mut self, path_str: &str) -> FileId {
        let path = Path::new(path_str);
        self.load_file_from_path(path)
    }

    pub fn current_file_id(&self) -> FileId {
        if let Some(current_file) = &self.current_file {
            current_file.id
        } else {
            unreachable!("There should always be a current file");
        }
    }

    pub fn report_error(&self, message: &str, span: Span, label: &str) {
        let id = self.current_file_id();
        let err = Diagnostic::new_error(message, Label::new(id, span, label));
        // TODO we should buffer diagnostics and report all at once
        // at the end of parsing
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(&mut writer.lock(), &config, &self.files, &err).unwrap();
    }
}

/// A source file represents everything we need to know about a single
/// file in the module system. It contains information about the file's
/// path, content, and dependencies.
#[derive(Clone, Debug)]
pub struct SourceFile {
    pub path: PathBuf,
    pub abs_path: PathBuf,
    pub id: FileId,
}

#[derive(Default)]
pub struct FileLoader {}

impl FileLoader {
    fn read_file(&self, path: &Path) -> io::Result<String> {
        fs::read_to_string(path)
    }

    fn to_absolute(&self, path: &Path) -> Option<PathBuf> {
        if path.is_absolute() {
            Some(path.to_path_buf())
        } else {
            env::current_dir().ok().map(|cwd| cwd.join(path))
        }
    }
}
