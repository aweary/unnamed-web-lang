// use crate::scope::ScopeTracker;

use codespan::{FileId, Files};
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use diagnostics::Diagnostic;
use fxhash::FxHashMap;

use petgraph::Graph;

use std::path::PathBuf;
use std::result::Result;

/// Compiling context, passed around and populated by different phases
/// of the compiler.
pub struct Context {
    /// Tracks scope during lowering. Not currently persisted after being lowered.
    // pub scope: ScopeTracker,
    /// Graph mapping dependencies between files
    pub module_graph: Graph<FileId, ()>,
    /// Tracks metadata about the current sequence of import paths, used  for
    /// cycle detection.
    pub import_path: Vec<PathBuf>,
    /// Used alongside import_path, gives us diagnostics when we do find a cycle.
    /// TODO this should probably be lazily constructed if possible?
    pub import_path_nodes: Vec<diagnostics::Label>,
    /// A cache that maps absolute file paths to their already-lowered module IR
    // pub path_to_module_id: FxHashMap<PathBuf, ModuleId>,
    /// Stopgap solution to getting the module graph ID from the import module ID
    pub module_graph_node_map: FxHashMap<FileId, petgraph::graph::NodeIndex>,
    /// The set of files read from the file system
    pub files: Files,
}

impl Context {
    pub fn new() -> Context {
        Context {
            // scope: ScopeTracker::default(),
            module_graph: Graph::default(),
            import_path: Default::default(),
            import_path_nodes: Default::default(),
            // path_to_module_id: FxHashMap::default(),
            module_graph_node_map: FxHashMap::default(),
            files: Files::new(),
        }
    }

    pub fn add_file(&mut self, path: &PathBuf) -> Result<FileId, String> {
        // We require that file paths be absolute before adding, so that we
        // can associate their file IDs correctly
        if !path.is_absolute() {
            panic!("add_file requires that `path` be absolute. Please resolve the path before calling add_file. This indiciates a bug in the compiler");
        }
        let files = &mut self.files;
        match std::fs::read_to_string(path.clone()) {
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
        // match self.file_id_map.get(path) {
        //     Some(file_id) => {
        //         Ok(*file_id)
        //     }
        //     None => {
        //     }
        // }
    }

    pub fn resolve_file(&self, id: FileId) -> &str {
        self.files.source(id)
    }

    pub fn emit_diagnostic(&self, diagnostic: Diagnostic) {
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(&mut writer.lock(), &config, &self.files, &diagnostic).unwrap();
    }
    
}
