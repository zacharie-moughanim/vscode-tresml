"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
var vscode = require("vscode");
// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
function activate(context) {
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('tresml is now active');
    // The command has been defined in the package.json file
    // Now provide the implementation of the command with registerCommand
    // The commandId parameter must match the command field in package.json
    var disposable = vscode.commands.registerCommand('tresml.foo', function () {
        // The code you place here will be executed every time your command is executed
        // Display a message box to the user
        vscode.window.showInformationMessage('foo was just run');
    });
    context.subscriptions.push(disposable);
}
// This method is called when your extension is deactivated
function deactivate() { }
///////////////////////////////////////////////////////////////////////////////////////////////////
// let client: LanguageClient | undefined;
// export async function activate(context: ExtensionContext) {
//   // The server is implemented in node
//   let serverModule = context.asAbsolutePath(path.join('server', 'out', 'server.js'));
//   // The debug options for the server
//   // --inspect=6009: runs the server in Node's Inspector mode so VS Code can attach to the server for debugging
//   let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };
//   // If the extension is launched in debug mode then the debug server options are used
//   // Otherwise the run options are used
//   let serverOptions: ServerOptions = {
//     run: { module: serverModule, transport: TransportKind.ipc },
//     debug: {
//       module: serverModule,
//       transport: TransportKind.ipc,
//       options: debugOptions
//     }
//   };
//   // Options to control the language client
//   let clientOptions: LanguageClientOptions = {
//     // Register the server for plain text documents
//     documentSelector: [{ scheme: 'file', language: 'plaintext' }],
//     synchronize: {
//       // Notify the server about file changes to '.clientrc files contained in the workspace
//       fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
//     }
//   };
//   // Create the language client and start the client.
//   client = new LanguageClient(
//     'languageServerExample',
//     'Language Server Example',
//     serverOptions,
//     clientOptions
//   );
//   // Start the client. This will also launch the server
//   await client.start();
// }
// export async function deactivate() {
//   await client?.dispose();
//   client = undefined;
// }
