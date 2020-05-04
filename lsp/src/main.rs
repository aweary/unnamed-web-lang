mod lsp;

fn main() {
    use lsp::LSPServer;
    LSPServer::new().start().unwrap();
}
