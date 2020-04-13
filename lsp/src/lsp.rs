use codespan_lsp::byte_span_to_range;
use log::info;
use lsp_server::{Connection, IoThreads, Message, Notification};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
        Notification as LSPNotification, PublishDiagnostics,
    },
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, InitializeParams, Position, PublishDiagnosticsParams,
    Range as LspRange, ServerCapabilities, TextDocumentItem, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url, TextDocumentIdentifier 
};
use serde_json;
use std::error::Error;
use std::sync::Arc;

use diagnostics::ParseResult as CompilerResult;
use source::diagnostics::Diagnostic as CompilerDiagnostic;
use source::diagnostics::{ByteIndex, Span};
use source::filesystem::{FileId, FileSystem, Files};

use compiler;

pub type GenericResult<T> = Result<T, Box<dyn Error + Sync + Send>>;

pub struct LSPServer {
    // connection: Connection,
    // io_threads: IoThreads,
    capabilities: ServerCapabilities,
    vfs: Arc<FileSystem>,
}

impl LSPServer {
    pub fn new() -> Self {
        // Set up logging. Because `stdio_transport` gets a lock on stdout and stdin, we must have
        // our logging only write out to stderr.
        flexi_logger::Logger::with_str("info").start().unwrap();
        info!("starting generic LSP server");

        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        let mut capabilities = ServerCapabilities::default();

        capabilities.text_document_sync =
            Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full));

        LSPServer {
            capabilities,
            vfs: Arc::new(FileSystem::new()),
        }
    }

    pub fn start(mut self) -> GenericResult<()> {
        let server_capabilities = serde_json::to_value(&self.capabilities).unwrap();
        // Create the transport. Includes the stdio (stdin and stdout) versions but this could
        // also be implemented to use sockets or HTTP.
        let (connection, io_threads) = Connection::stdio();
        let _initialization_params = connection.initialize(server_capabilities)?;
        self.main_loop(&connection)?;
        io_threads.join()?;
        Ok(())
    }

    fn main_loop(&mut self, connection: &Connection) -> GenericResult<()> {
        for msg in &connection.receiver {
            match msg {
                Message::Request(req) => {
                    if connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    info!("got request: {:?}", req.method);
                    // ...
                }
                Message::Response(resp) => {
                    info!("got response: {:?}", resp.id);
                }
                Message::Notification(notif) => match notif.method.as_str() {
                    DidOpenTextDocument::METHOD => {
                        let params: DidOpenTextDocumentParams =
                            notif.extract(DidOpenTextDocument::METHOD).unwrap();
                        self.on_open_text_document(params, connection);
                    }
                    DidChangeTextDocument::METHOD => {
                        let params: DidChangeTextDocumentParams =
                            notif.extract(DidChangeTextDocument::METHOD).unwrap();
                        self.on_change_text_document(params);
                    }
                    DidCloseTextDocument::METHOD => {
                        let params: DidCloseTextDocumentParams =
                            notif.extract(DidCloseTextDocument::METHOD).unwrap();
                        self.on_close_text_document(params, connection);
                    }
                    _ => {}
                },
            }
        }
        Ok(())
    }

    fn on_change_text_document(&mut self, params: DidChangeTextDocumentParams) {
        info!("text document change: {:#?}", params);
        let text = params.text_document;
        // ...
    }

    fn on_open_text_document(
        &mut self,
        params: DidOpenTextDocumentParams,
        connection: &Connection,
    ) -> GenericResult<()> {
        info!("text document open: {:#?}", params);
        let TextDocumentItem { uri, text, .. } = params.text_document;
        // Conver the URI to a path, which our filesystem uses
        let path = uri.to_file_path().unwrap();
        // Add this text document to the VFS
        // TODO don't unwrap here
        let file = self.vfs.load(&path, text).unwrap();
        // Compile the module and report any errors
        match compiler::run_on_file(self.vfs.clone(), file) {
            Ok(_) => {}
            Err(diagnostic) => {
                self.report_diagnostic(connection, diagnostic, uri, self.vfs.clone(), file)?;
                // ...
            }
        }
        Ok(())
    }

    fn report_diagnostic(
        &self,
        connection: &Connection,
        diagnostic: CompilerDiagnostic,
        url: Url,
        vfs: Arc<FileSystem>,
        file: FileId,
    ) -> GenericResult<()> {
        let mut lsp_diagnostics = vec![];
        let labels = diagnostic.labels;

        // Build up the set of LSP diagnostics from the codespan diagnostic struct
        for label in labels {
            let range = {
                let start_location = vfs.location(file, label.range.start).unwrap();
                let end_location = vfs.location(file, label.range.end).unwrap();
                let start_position = Position {
                    line: start_location.line_number as u64 - 1,
                    character: start_location.column_number as u64 - 1,
                };
                let end_position = Position {
                    line: end_location.line_number as u64 - 1,
                    character: end_location.column_number as u64 - 1,
                };
                LspRange::new(start_position, end_position)
            };
            let message = label.message;
            let lsp_diagnostic = Diagnostic::new_simple(range, message);
            lsp_diagnostics.push(lsp_diagnostic);
        }

        // Parameters for the "textDocument/publishDiagnostics" notification
        let params = PublishDiagnosticsParams::new(url, lsp_diagnostics, None);
        // Notification to send to the LSP client
        let notification = Notification::new(
            PublishDiagnostics::METHOD.to_owned(),
            serde_json::to_value(&params).unwrap(),
        );
        connection
            .sender
            .send(Message::Notification(notification))?;
        Ok(())
        // Send the notification
    }

    fn on_close_text_document(
        &mut self,
        params: DidCloseTextDocumentParams,
        connection: &Connection,
    ) -> GenericResult<()> {
        let TextDocumentIdentifier { uri } = params.text_document;
        // Parameters for the "textDocument/publishDiagnostics" notification
        let params = PublishDiagnosticsParams::new(uri, vec![], None);
        // Notification to send to the LSP client
        let notification = Notification::new(
            PublishDiagnostics::METHOD.to_owned(),
            serde_json::to_value(&params).unwrap(),
        );
        connection
            .sender
            .send(Message::Notification(notification))?;
        info!("text document close: {:#?}", params);
        Ok(())
    }
}

fn compile_document(text: &str) {
    // ...
}

fn cast_notif<N>(notif: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notif.extract(N::METHOD)
}
