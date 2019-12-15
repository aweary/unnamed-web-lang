pub mod files;

use codespan::{FileId, Files};
use files::absolute_path;
use salsa::{self, InternId, InternKey};

use std::sync::Arc;
use std::sync::Mutex;

use fxhash::hash32;
use std::fs;
use std::path::PathBuf;

// use relative_path::{RelativePathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ModuleId(pub u32);

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
    files.add(path.to_str().unwrap(), source)
}

// ....
