use std::path::PathBuf;

use diagnostics::ParseResult as Result;

use source::diagnostics::{Diagnostic, Label, LabelStyle};
use source::filesystem::FileId;

use data_structures::module_graph::{ModuleGraph};
use data_structures::HashSet;
use diagnostics::ParseResult;
use lowering::LoweringContext;
use parser::Parser;

use crossbeam::deque::{Injector, Worker};

use hir;
use hir::visit::{walk_module, Visitor};
use syntax::ast;
use syntax::symbol::Symbol;

use std::sync::Arc;

use std::hash::{Hash, Hasher};

use source::FileSystem;

type ImportDescriptorList = Vec<(PathBuf, hir::Ident, ast::ImportPath)>;

type DashSet<T> = dashmap::DashMap<T, ()>;

type HirModuleGraph = ModuleGraph<hir::Module, Symbol>;

#[derive(Clone, Debug)]
struct GloballyUniqueImport {
    import: hir::Import,
    file: FileId,
}

impl PartialEq for GloballyUniqueImport {
    fn eq(&self, rhs: &Self) -> bool {
        self.import.name.name == rhs.import.name.name
            && self.import.path.resolved == rhs.import.path.resolved
    }
}

impl Eq for GloballyUniqueImport {}

impl GloballyUniqueImport {
    pub fn new(import: &hir::Import, file: FileId) -> Self {
        GloballyUniqueImport {
            import: import.clone(),
            file,
        }
    }
}

/// A custom Hash implementation that ignores the file, which is
/// module specific data used only for error reporting.
impl Hash for GloballyUniqueImport {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.import.name.name.hash(state);
        self.import.path.resolved.hash(state);
    }
}

struct ImportResolver {
    root_file: FileId,
    seen_modules: HashSet<PathBuf>,
    imported_modules: HashSet<GloballyUniqueImport>,
}

fn report_import_error(
    import_module: &hir::Module,
    unique_import: &GloballyUniqueImport,
) -> Diagnostic<FileId> {
    let import = &unique_import.import;
    let mut labels = vec![
        Label::new(LabelStyle::Primary, unique_import.file, import.name.span)
            .with_message("Import not found"),
    ];
    // Try and figure out what the user might have wanted...
    if let Some(similar_def) = import_module.resolve_similar_export(&import.name) {
        let message = if similar_def.visibility == hir::DefinitionVisibility::Private {
            if similar_def.name().name == import.name.name {
                "An item with this name exists, but it isn't exported."
            } else {
                "This export looks similar, maybe you have a typo? It's also private, so make sure to export it."
            }
        } else {
            "This export looks similar, maybe you have a typo?"
        };
        labels.push(
            Label::new(
                LabelStyle::Secondary,
                import_module.file,
                similar_def.name().span,
            )
            .with_message(message),
        );
    }
    Diagnostic::error()
        .with_message("Cannot resolve_import")
        .with_labels(labels)
}

impl ImportResolver {
    fn new(root_file: FileId) -> Self {
        ImportResolver {
            root_file,
            seen_modules: HashSet::default(),
            imported_modules: HashSet::default(),
        }
    }

    /// Take the root HIR module and populate the module graph
    pub fn populate_module_graph(
        mut self,
        module: &hir::Module,
    ) -> Result<HashSet<GloballyUniqueImport>> {
        // Walk the root module
        self.visit_module(module)?;
        Ok(self.imported_modules)
    }
}

impl Visitor for ImportResolver {
    fn visit_module(&mut self, module: &hir::Module) -> Result<()> {
        // println!("Visiting module {:?}", module.file);
        // Reset the imported modules list
        self.imported_modules = HashSet::default();
        self.root_file = module.file;
        walk_module(self, module)?;
        Ok(())
    }

    fn visit_import(&mut self, import: &hir::Import) -> Result<()> {
        // When we visit an import, we don't actually follow the path yet.
        // We just want to record that this module is importing a specific value
        // from a specific file. Then we take the path for that file, and the name
        // of the value being imported, and we add that to a set of of imports
        // that will be processed separately. That way we can do the I/O and lowering
        // work for each module concurrently, rather than doing that here.
        // println!(
        //     "Visiting import: {:?} from {:?}",
        //     import.name, import.path.resolved
        // );
        let unique_import = GloballyUniqueImport::new(&import, self.root_file);
        self.imported_modules.insert(unique_import);
        Ok(())
    }
}

use std::iter;

fn find_task(
    local: &Worker<GloballyUniqueImport>,
    global: &Injector<GloballyUniqueImport>,
) -> Option<GloballyUniqueImport> {
    // Pop a task from the local queue, if not empty.
    local.pop().or_else(|| {
        // Otherwise, we need to look for a task elsewhere.
        iter::repeat_with(|| {
            // Try stealing a batch of tasks from the global queue.
            global.steal_batch_and_pop(local)
        })
        // Loop while no task was stolen and any steal operation needs to be retried.
        .find(|s| !s.is_retry())
        // Extract the stolen task, if there is one.
        .and_then(|s| s.success())
    })
}

fn parse_and_lower_module_from_file(
    file: FileId,
    vfs: Arc<FileSystem>,
) -> ParseResult<hir::Module> {
    let ast = vfs.with_source(&file, |source| -> ParseResult<ast::Module> {
        let mut parser = Parser::new(source, file);
        let ast = parser.parse_module()?;
        Ok(ast)
    })?;
    let hir = LoweringContext::new(vfs, file).lower_module(ast)?;
    Ok(hir)
}

fn run_from_source_root_impl(path: PathBuf, vfs: Arc<FileSystem>) -> ParseResult<()> {
    let start = std::time::Instant::now();
    // Resolve the path assuming main.dom is the entry point.
    let path = &path.join("main.dom");
    // Read the file out of the shared file system
    let root_file = vfs.resolve(&path)?;
    let hir = parse_and_lower_module_from_file(root_file, vfs.clone())?;
    // Get the initial set of imports
    let imports = ImportResolver::new(root_file).populate_module_graph(&hir)?;
    let seen_modules = Arc::new(DashSet::new());

    // Create a global, work-stealing queue for processing the imports. Populate
    // with the initial set of imports.
    let injector = {
        let injector = Injector::new();
        for import in imports {
            seen_modules.insert(import.clone(), ());
            injector.push(import);
        }
        Arc::new(injector)
    };

    use std::sync::Mutex;

    let module_graph = Arc::new(Mutex::new(HirModuleGraph::default()));

    // All the threads we spawn for this
    let mut handles = vec![];

    for _ in 0..num_cpus::get() {
        // Thread-local copy of the virtual filesystem
        let vfs = vfs.clone();
        // Thread-local copy of the global scheduler queue
        let injector = injector.clone();
        // Thread-local copy of the seen modules set, should be folded into the module graph
        let seen_modules = seen_modules.clone();
        // Module graph
        let module_graph = module_graph.clone();
        let handle = std::thread::spawn(move || -> ParseResult<()> {
            let queue = Worker::new_fifo();
            while let Some(unique_import) = find_task(&queue, &injector) {
                let path = &unique_import.import.path.resolved;
                let file = vfs.resolve(path).map_err(|err| {
                    let message = err.message.clone();
                    err.with_labels(vec![Label::primary(
                        unique_import.file,
                        unique_import.import.path.span,
                    )
                    .with_message(message)])
                })?;
                // let file = vfs.resolve(path).unwrap();
                let hir = parse_and_lower_module_from_file(file, vfs.clone())?;
                if hir.resolve_export(&unique_import.import.name).is_none() {
                    return Err(report_import_error(&hir, &unique_import));
                }
                let imports = ImportResolver::new(file).populate_module_graph(&hir)?;
                // TODO
                let _module_id = module_graph.lock().unwrap().add(hir);
                // TODO validate imports...
                for import in imports {
                    if !seen_modules.contains_key(&import) {
                        seen_modules.insert(import.clone(), ());
                        injector.push(import);
                    }
                }
            }
            Ok(())
        });
        handles.push(handle);
    }

    for handle in handles {
        match handle.join().unwrap() {
            Ok(_) => {

            },
            Err(diagnostic) => {
                report_diagnostic(diagnostic, &*vfs);
            }
        }
    }

    println!(
        "Completed parsing, name, and crawling imports for {} modules in {}μs",
        seen_modules.len(),
        start.elapsed().as_micros()
    );
    Ok(())
}

fn report_diagnostic(diagnostic: Diagnostic<FileId>, files: &FileSystem) {
    use source::diagnostics::{
        emit,
        termcolor::{ColorChoice, StandardStream},
        Config,
    };
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    emit(&mut writer.lock(), &config, &*files, &diagnostic).expect("Emitting");
}

/// Run the compiler, pointing at a directory. We expect
/// to resolve {dir}/main.dom as the entrypoint.  
pub fn run_from_source_root(path: PathBuf) -> ParseResult<()> {
    // The shared in-memory file system
    let files = Arc::new(FileSystem::new());
    match run_from_source_root_impl(path, files.clone()) {
        Ok(_) => {
            // Successful run, we can exit
        }
        Err(diagnostic) =>report_diagnostic(diagnostic, &files) 
    };
    Ok(())
}
