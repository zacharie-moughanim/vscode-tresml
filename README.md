# TresML VSCode extension

Official [TresML](https://github.com/Ilianmgh/tresfreroserver.git) language extension for VSCode.

## Features

### Building HTML file from TresML file

This extension provides a command to compile TresML files directly to HTML files, without having to interact with a server.

The extension bundles two versions of the TresML interpreter: an OCaml bytecode that requires `ocamlrun` (see [dependencies](#Dependencies)) or native-compiled that is only available on x86_64 architecture for Windows and Unix systems. You can choose the version you wish in the extension's settings.

However, the possibility to directly compile a TresML file into a HTML page naturally limits the features of the TresML language: since the page is not interacting with a web server features like redirection, session variables are not available when compiling this way.

### Syntax highlighting for TresML source files

This extension allows to preserve HTML syntax highlighting at top level, while correctly highlighting code within TresML code brackets `<{ ... }>`.

Example:

With a dark mode:

[![Syntax highlighting example dark theme](./images/syntax_highlighting_dark_example.png)](https://github.com/Ilianmgh/tresfreroserver/blob/main/html/test_ml/showcase_test.tml)

With a light mode:

[![Syntax highlighting example light theme](./images/syntax_highlighting_light_example.png)](https://github.com/Ilianmgh/tresfreroserver/blob/main/html/test_ml/showcase_test.tml)

<!-- ## Requirements -->

<!-- ## Extension Settings -->

## Dependencies

If you use the command to compile TresML files and choose to compile using the bytecode-compiled version of the TresML interpreter, you need to have an [OCaml bytecode interpreter](https://ocaml.org/install) installed.

## Known Issues

- The TresML intepreter binaries only target an x86_64 architecture for now. The extension provides executables for Windows (compiled with an [OCaml cross-compiler targetting Windows](https://github.com/ocaml-cross/opam-cross-windows)) and Unix systems (compiled with ocamlopt).
- Syntax highlighting is misleading when opening tresml brackets within e.g. strings, comments or even HTML comments. Can't seem to allow embedded tresml code to be correctly highlighted within e.g. javascript strings while not highlighted within comments. Semantically, tresml code within javascript strings is evaluated whereas it's not evaluated within tresml comments. Should switch to semantic highlighting to fix maybe.