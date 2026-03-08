import * as vscode from 'vscode';
// import * as path from 'path';
// import { workspace, ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

const platform : string = process.platform;
let filePathSeparator : string; // The separator between folder along a path ; it's a slash '/' for Unix-like system and a backslash '\' for windows. 

var exec = require('child_process').exec;
let debugChannel	: vscode.OutputChannel;
const noWorkDirErrorMessage : string = "No working directory found, try opening a directory.";

export function activate(context: vscode.ExtensionContext) {
	if (platform === 'win32') {
		filePathSeparator = '\\';
	} else { // Since values of platform can be aix, darwin, freebsd, linux, openbsd, sunos or win32 and that, except Windows, all these are Unix-like and use / as a path separator, we assume that all non-Windows supported platform uses '/' as a path separator.
		filePathSeparator = '/';
	}

	// Setting up the debug channel
	debugChannel = vscode.window.createOutputChannel("TresML Debug", {log : true});

	// Current working directory, if it exists
	let workDir : string | undefined = undefined; // Actually at some point e.g. when we allow multi-file TresML projects.
	// Directory of the extension
	let extensionDir : string = context.extensionPath;

	// Building TresML interpreter
	exec (`cd ${extensionDir}${filePathSeparator}tresml_interpreter && make`, (error : any, stdout : any, stderr : any) => {	
		if (error) {
			const errorMessage : string = `Error occured while compiling TresML: ${error}`;
			debugChannel.appendLine(errorMessage);
			debugChannel.show(true);
			console.error(errorMessage);
			return;
		}
		if (stderr) {
			const stderrMessage : string = `Error messages during compilation of TresML: ${stderr}`;
			debugChannel.appendLine(stderrMessage);
			console.error(stderrMessage);
		}
		const outputMessage : string = `TresML interpreter correctly compiled (or was already compiled) output of compilation:\n${stdout}`;
		debugChannel.appendLine(outputMessage);
		console.log(outputMessage);
	});

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
				const fileToCompile : string = vscode.window.activeTextEditor.document.uri.path;
				const sourceFileParentDir : string = fileToCompile.replace(/\/.*.tml$/i, "");
				vscode.window.showInformationMessage(sourceFileParentDir);
				const outputFile : string = fileToCompile.replace(/.tml$/i, ".html"); // output file is the same file name with .html extension instead of .tml
				exec (`cd ${sourceFileParentDir} && ${extensionDir}${filePathSeparator}tresml_interpreter${filePathSeparator}produce_page.x ${fileToCompile} ${outputFile} -noServerData`, (error : any, stdout : any, stderr : any) => {	
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
			} else {
				vscode.window.showErrorMessage("No active file in text editor when TresML is called. Select an active file to compile.");
			}
	});

  context.subscriptions.push(disposable_tml_compile);
}

export function deactivate() {}