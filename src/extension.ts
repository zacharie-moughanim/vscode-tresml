import * as vscode from 'vscode';
// import * as path from 'path';
// import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

const path = require('path');
const platform : string = process.platform;
var exec = require('child_process').exec;

let debugChannel	: vscode.OutputChannel;
const noWorkDirErrorMessage : string = "No working directory found, try opening a directory.";

let filePathSeparator : string; // The separator between folder along a path ; it's a slash '/' for Unix-like system and a backslash '\' for windows. 
let osName : string;
let exeExtension : string;

export function activate(context: vscode.ExtensionContext) {
	if (platform === 'win32') {
		filePathSeparator = '\\';
		osName = "Windows";
		exeExtension = "exe";
	} else { // Since values of platform can be aix, darwin, freebsd, linux, openbsd, sunos or win32 and that, except Windows, all these are Unix-like and use / as a path separator, we assume that all non-Windows supported platform uses '/' as a path separator.
		filePathSeparator = '/';
		osName = "Unix";
		exeExtension = "x";
	}

	// Setting up the debug channel
	debugChannel = vscode.window.createOutputChannel("TresML Debug", {log : true});

	// Current working directory, if it exists
	let workDir : string | undefined = undefined; // Actually at some point e.g. when we allow multi-file TresML projects.
	// Directory of the extension
	let extensionDir : string = context.extensionPath;

	if(vscode.workspace.workspaceFolders !== undefined) {
		workDir = vscode.workspace.workspaceFolders[0].uri.path;
	} else {
		debugChannel.appendLine(noWorkDirErrorMessage);
		console.error(noWorkDirErrorMessage);
	}

	const disposable_tml_compile = vscode.commands.registerCommand('tresml.compile', () => {
		if(workDir === undefined) {
			if(vscode.workspace.workspaceFolders !== undefined) { // if at the current call we found a working directory
				workDir = vscode.workspace.workspaceFolders[0].uri.path;
			} else {
				debugChannel.appendLine(noWorkDirErrorMessage);
				debugChannel.show(true);
				console.error(noWorkDirErrorMessage);
			}
		}
		
		if (vscode.window.activeTextEditor !== undefined) {
				const fileToCompile : string = vscode.window.activeTextEditor.document.uri.fsPath;
				const sourceFileParentDir : string = path.dirname(fileToCompile);
				const outputFile : string = fileToCompile.replace(/.tml$/i, ".html"); // output file is the same file name with .html extension instead of .tml
				debugChannel.appendLine(sourceFileParentDir);
				debugChannel.appendLine(fileToCompile);
				debugChannel.show(true);
				exec (`cd ${sourceFileParentDir} && ${extensionDir}${filePathSeparator}tresml_interpreter${filePathSeparator}bin${filePathSeparator}${osName}${filePathSeparator}x86_64${filePathSeparator}produce_page.${exeExtension} ${fileToCompile} ${outputFile} -noServerData`, (error : any, stdout : any, stderr : any) => {	
					if (error) {
						const errorMessage : string = `Error occured during HTML output of TresML file: ${error}`;
						debugChannel.appendLine(errorMessage);
						debugChannel.show(true);
						console.error(errorMessage);
						return;
					}
					if (stderr) {
						const stderrMessage : string = `Error messages during HTML output of TresML file: ${stderr}`;
						debugChannel.appendLine(stderrMessage);
						console.error(stderrMessage);
					}
					if (stdout) {
						const outputMessage : string = `Output of HTML output of TresML file: ${stdout}`;
						debugChannel.appendLine(outputMessage);
						console.log(outputMessage);
					}
				});
				// const simpleBrowserExt : vscode.Extension<any> | undefined = vscode.extensions.getExtension("vscode.simple-browser");

				// let simpleBrowerCmd : string = "simpleBrowser.show";

				// if (simpleBrowserExt !== undefined) {
				// 	if( simpleBrowserExt.isActive === false ){
				// 		simpleBrowserExt.activate().then(
				// 			function(){
				// 				console.log( "Extension activated");
				// 				vscode.commands.executeCommand(simpleBrowerCmd, [outputFile]);
				// 			},
				// 			function(){
				// 				console.log( "Extension activation failed");
				// 			}
				// 		);   
				// 	} else {
				// 		vscode.commands.executeCommand(simpleBrowerCmd, [outputFile]);
				// 	}
				// }

			} else {
				vscode.window.showErrorMessage("No active file in text editor when TresML is called. Select an active file to compile.");
			}
	});

  context.subscriptions.push(disposable_tml_compile);
}

export function deactivate() {}