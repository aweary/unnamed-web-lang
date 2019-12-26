pub mod files;

use codespan::{FileId, Files};
use salsa::{self, InternId, InternKey};

use std::sync::Arc;
use std::sync::Mutex;

use std::fs;
use std::path::PathBuf;

use fxhash;


// use relative_path::{RelativePathBuf};

// We generate module IDs from paths. They can be the keys for salsa::intern?
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

impl InternKey for ModuleId {
    fn from_intern_id(v: InternId) -> Self {
        ModuleId(v.as_u32())
    }

    fn as_intern_id(&self) -> InternId {
        InternId::from(self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceRoot {
    path: PathBuf,
}

#[salsa::query_group(SourceDatabase)]
pub trait Source: salsa::Database {
    #[salsa::input]
    fn files(&self, (): ()) -> Arc<Mutex<Files>>;
    #[salsa::input]
    fn current_path(&self, (): ()) -> PathBuf;
    fn resolve_file(&self, path: PathBuf) -> FileId;
    fn source_text(&self, file_id: FileId) -> String;
    // ...
}

fn source_text(db: &impl Source, file: FileId) -> String {
    let files_db = db.files(());
    let files = files_db.lock().unwrap();
    files.source(file).to_string()
}

fn resolve_file(db: &impl Source, path: PathBuf) -> FileId {
    println!("resolve_file {:?}", path);
    let files_db = db.files(());
    let mut files = files_db.lock().unwrap();
    // TODO don't unwrap here
    let source = fs::read_to_string(path.clone()).unwrap();
    let source_hash = fxhash::hash(&source);
    println!("source hash {:?}", source_hash);
    files.add(path.to_str().unwrap(), source)
}

// ....
