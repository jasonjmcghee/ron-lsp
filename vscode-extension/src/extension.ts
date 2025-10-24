import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  // Get server path from configuration
  const config = workspace.getConfiguration('ronLsp');
  const serverPath = config.get<string>('serverPath', 'ron-lsp');

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: [],
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'ron' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.ron'),
    },
  };

  client = new LanguageClient(
    'ronLsp',
    'RON Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
