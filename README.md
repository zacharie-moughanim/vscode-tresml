# TresML VSCode extension

Official [TresML](https://github.com/Ilianmgh/tresfreroserver.git) language extension for VSCode.

## Features

### Building HTML file from TresML file

This extension provides a command to compile TresML files directly to HTML files, without having to interact with a server; the extension bundles a pre-compiled TresML interpreter.

However, the possibility to directly compile a TresML file into a HTML page naturally limits the features of the TresML language: since the page is not interacting with a web server features like redirection, session variables are not available when compiling this way.

### Syntax highlighting for TresML source files

This extension allows to preserve HTML syntax highlighting at top level and within TresML code, while correctly highlighting code within TresML code brackets `<{ ... }>`.

Example:

With a dark mode:

[![Syntax highlighting example dark theme](./images/syntax_highlighting_dark_example.png)](https://github.com/Ilianmgh/tresfreroserver/blob/main/html/test_ml/showcase_test.tml)

With a light mode:

[![Syntax highlighting example light theme](./images/syntax_highlighting_light_example.png)](https://github.com/Ilianmgh/tresfreroserver/blob/main/html/test_ml/showcase_test.tml)

<!-- ## Requirements -->

<!-- ## Extension Settings -->

## Known Issues

- The TresML intepreter binaries target an x86_64 architecture. The extension provides executables for Windows (compiled with an [OCaml cross-compiler targetting Windows](https://github.com/ocaml-cross/opam-cross-windows)) and Unix systems (compiled with ocamlopt).