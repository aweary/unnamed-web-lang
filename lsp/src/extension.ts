import * as path from "path";
import { workspace, ExtensionContext } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from "vscode-languageclient";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // TODO read this from config
  const serverPath = path.resolve("/Users/brandondail/Code/compiler/target/debug/lsp");

  // The debug options for the server
  // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
  let debugOptions = {};

  // If the extension is launched in debug mode then the debug server options are used
  // Otherwise the run options are used
  let serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
    options: {
      env: {
        "RUST_BACKTRACE": "1",
        "RUST_LOG": "lsp_server=debug"
      }
    }
  };

  // Options to control the language client
  let clientOptions: LanguageClientOptions = {
    // Register the server for plain text documents
    documentSelector: [{ scheme: "file", language: "webscript" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspace
      fileEvents: workspace.createFileSystemWatcher("**/.clientrc")
    }
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "webscriptLSP",
    "WebScript Language Server Client",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
