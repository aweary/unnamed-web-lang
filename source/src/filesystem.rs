pub use codespan_reporting::files::{Files, SimpleFile};

use crate::diagnostics::Diagnostic;

use std::fmt::{self, Display};
use std::fs;
use std::ops::Range;
use std::path::PathBuf;

use lsp_types::Url;

use dashmap::DashMap;

use crossbeam::atomic::AtomicCell;



pub type Result<T> = std::result::Result<T, Diagnostic>;


#[derive(Debug, Clone, Hash)]
pub enum FileIdentifier  {
    Path(PathBuf),
    Url(Url),
}

#[derive(Debug, PartialEq, Clone, Copy, Hash, Eq)]
pub struct FileId(usize);

#[derive(Debug, Clone)]
pub struct FileName(PathBuf);

impl Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

type File = SimpleFile<FileName, String>;

/// The core data structure for interacting with source text from
/// the filesystem. It's meant to make concurrent reads/writes easier
/// to deal with as we populate this in-memory
pub struct FileSystem {
    next_id: AtomicCell<usize>,
    files: DashMap<FileId, File>,
    ids: DashMap<PathBuf, FileId>,
    paths: DashMap<FileId, PathBuf>,
}

impl FileSystem {
    pub fn new() -> Self {
        FileSystem {
            next_id: AtomicCell::new(0),
            // TODO how to use fxhash here?
            files: DashMap::default(),
            ids: DashMap::default(),
            paths: DashMap::default(),
        }
    }

    pub fn id_for_path(&self, path: &PathBuf) -> Option<FileId> {
        self.ids.get(path).map(|path| *path)
    }

    pub fn path_for_id(&self, id: &FileId) -> PathBuf {
        self.paths
            .get(id)
            .expect("Expect path for id")
            .to_path_buf()
    }

    pub fn update(&self, id: FileId, text: String) {
        self.files
            .alter(&id, |_, file| File::new(file.name().clone(), text))
    }

    pub fn load(&self, path: &PathBuf, text: String) -> Result<FileId> {
        let name = FileName(path.clone());
        let file = File::new(name, text);
        // Atomically increment the file id counter
        let id = {
            let id = self.next_id.load();
            self.next_id.store(id + 1);
            id
        };
        let fileid = FileId(id);
        self.files.insert(fileid, file);
        self.ids.insert(path.clone(), fileid);
        self.paths.insert(fileid, path.clone());
        Ok(fileid)
    }

    pub fn resolve(&self, path: &PathBuf) -> Result<FileId> {
        if let Some(fileid) = self.id_for_path(path) {
            Ok(fileid)
        } else {
            // Atomically increment the file id counter
            let id = {
                let id = self.next_id.load();
                self.next_id.store(id + 1);
                id
            };
            let fileid = FileId(id);
            let source = match fs::read_to_string(path) {
                Ok(source) => source,
                Err(os_err) => {
                    return Err(Diagnostic::error().with_message(os_err.to_string()));
                }
            };
            let name = FileName(path.clone());
            let file = File::new(name, source);
            self.files.insert(fileid, file);
            self.ids.insert(path.clone(), fileid);
            self.paths.insert(fileid, path.clone());
            Ok(fileid)
        }
    }

    pub fn with_source<F, T, E>(&self, id: &FileId, func: F) -> std::result::Result<T, E>
    where
        F: FnOnce(&str) -> std::result::Result<T, E>,
    {
        let file = self.files.get(id).expect("Expect a source for a FileId");
        let value = func(&file.source())?;
        Ok(value)
    }
}

impl<'a> Files<'a> for FileSystem {
    type FileId = FileId;
    type Name = FileName;
    type Source = String;

    fn name(&self, id: FileId) -> Option<Self::Name> {
        let file = self.files.get(&id).unwrap();
        Some(file.name().clone())
    }

    fn source(&self, id: FileId) -> Option<Self::Source> {
        let file = self.files.get(&id).unwrap();
        Some(file.source().clone())
    }

    fn line_index(&self, file_id: FileId, byte_index: usize) -> Option<usize> {
        self.files.get(&file_id).unwrap().line_index((), byte_index)
    }

    fn line_range(&'a self, file_id: FileId, line_index: usize) -> Option<Range<usize>> {
        self.files.get(&file_id).unwrap().line_range((), line_index)
    }
}
