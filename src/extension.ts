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
const noWorkDirErrorMessage : string = "No working directory found. Defaulting to parent directory of opened file.";

// Current working directory, if it exists
let workDir : string | undefined = undefined; // Actually use at some point e.g. when we allow multi-file TresML projects.

let filePathSeparator : string; // The separator between folder along a path ; it's a slash '/' for Unix-like system and a backslash '\' for windows. 
let osName : string;
let exeExtension : string;
let architecture : string | undefined;
let interpreterVersion : string | undefined;
const nativeIntepreterFolder : string = "Native";
const bytecodeIntepreterFolder : string = "Bytecode";

/// Updates global variables from settings.json and contextual values (e.g. current working directory).
function updateSettings() {
	// Fetching the architecture and selected intepreter version's from settings
	const configArchi : string | undefined = vscode.workspace.getConfiguration('TresML').get("tmlInterpreter.machineArchitecture");
	const configInterpreterVersion : string | undefined = vscode.workspace.getConfiguration('TresML').get("tmlInterpreter.version");
	if (configArchi !== undefined) {
		architecture = configArchi;
	}
	if (configInterpreterVersion !== undefined) {
		interpreterVersion = configInterpreterVersion;
	}
	// Setting current working directory
	if(workDir === undefined) {
		if(vscode.workspace.workspaceFolders !== undefined) { // if at the current call we found a working directory
			workDir = vscode.workspace.workspaceFolders[0].uri.path;
		} else {
			debugChannel.appendLine(noWorkDirErrorMessage);
			vscode.window.showWarningMessage(noWorkDirErrorMessage);
		}
	}
}

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

	// Directory of the extension
	let extensionDir : string = context.extensionPath;
	
	updateSettings();

	const disposable_tml_compile = vscode.commands.registerCommand('tresml.compile', () => {
		updateSettings();

		if (interpreterVersion === undefined) {
			vscode.window.showErrorMessage("Couldn't compile TresML file: The interpreter version setting is undefined, please select one in the settings.");
			return;
		} else {
			if (interpreterVersion === nativeIntepreterFolder) {
				if (architecture === undefined) {
					vscode.window.showErrorMessage("Couldn't compile TresML file: The native compiled intepreter is selected, but the architecture setting is undefined. please select one in the settings.");
					return;
				}
			}
		}
		
		if (vscode.window.activeTextEditor !== undefined) {
				const fileToCompile : string = vscode.window.activeTextEditor.document.uri.fsPath;
				const sourceFileParentDir : string = path.dirname(fileToCompile);
				const outputFile : string = fileToCompile.replace(/.tml$/i, ".html"); // output file is the same file name with .html extension instead of .tml
				debugChannel.appendLine(`Compiling ${fileToCompile} to ${outputFile}`);
				let interpreterCmd : string = "echo \"Something went wrong, the interpreter version is not defined where it should be.\" ; false"; // Will always fail if the version of the interpreter is undefined (this would be a bug)
				if (interpreterVersion === nativeIntepreterFolder) {
					interpreterCmd = `${extensionDir}${filePathSeparator}tresml_interpreter${filePathSeparator}bin${filePathSeparator}${nativeIntepreterFolder}${filePathSeparator}${osName}${filePathSeparator}${architecture}${filePathSeparator}produce_page.${exeExtension}`;
				} else if (interpreterVersion === bytecodeIntepreterFolder) {
					interpreterCmd = `opam exec -- ocamlrun ${extensionDir}${filePathSeparator}tresml_interpreter${filePathSeparator}bin${filePathSeparator}${bytecodeIntepreterFolder}${filePathSeparator}produce_page.bytecode`;
				}
				debugChannel.appendLine(`Executing:\n${interpreterCmd} ${fileToCompile} ${outputFile} -noServerData`);
				exec (`${interpreterCmd} ${fileToCompile} ${outputFile} -noServerData`, (error : any, stdout : any, stderr : any) => {
					if (error) {
						const errorMessage : string = `Error occured during HTML output of TresML file: ${error}`;
						debugChannel.appendLine(errorMessage);
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