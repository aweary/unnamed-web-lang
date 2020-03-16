use source::{FileId, Files};

use std::fs;
use std::path::PathBuf;

use syntax::symbol::Symbol;

use std::sync::Arc;

use data_structures::HashMap;

/// Marker trait for data types that can be referenced
/// in code. Local variables, functions, enums, types, etc.
pub trait Referenceable {}

pub struct NameMap<T: Referenceable> {
    _data: HashMap<Symbol, Arc<T>>
}

pub struct Session {
    pub files: Files,
}

impl Default for Session {
    fn default() -> Session {
        Session { files: Files::new() }
    }
}

impl Session {
    /// Add a file to the parsing session
    pub fn add_file(&mut self, path: &PathBuf) -> Result<FileId, String> {
        // We require that file paths be absolute before adding, so that we
        // can associate their file IDs correctly
        if !path.is_absolute() {
            panic!("add_file requires that `path` be absolute. Please resolve the path before calling add_file. This indiciates a bug in the compiler");
        }
        let files = &mut self.files;
        match fs::read_to_string(path.clone()) {
            Ok(source) => {
                let file_id = files.add(path.to_str().unwrap(), source);
                // Associate the File ID with the absolute path...
                // self.file_id_map.insert(path.clone(), file_id);
                Ok(file_id)
            }
            Err(err) => {
                // ...
                Err(format!("{}", err))
            }
        }
    }

    /// Resolve the source for a previously added file
    pub fn resolve_file(&self, id: FileId) -> &str {
        self.files.source(id)
    }
}
