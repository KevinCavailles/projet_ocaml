Bi-partite matching based on a Gusacker Gowen algorithm with a Bellman Ford algorithm to search the shortest path.
This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

To use, you should install the *OCaml* extension in VSCode. Other extensions might work as well but make sure there is only one installed.
Then open VSCode in the root directory of this repository (command line: `code path/to/ocaml-maxflow-project`).

Features :
 - full compilation as VSCode build task (Ctrl+Shift+b)
 - highlights of compilation errors as you type
 - code completion
 - automatic indentation on file save


A makefile provides some useful commands:
 - `make build` to compile an algorithm which will accept an advanced file entry. This creates an ftest_advanced.native executable.
 - `make advanced` to compile an algorithm which will accept an advanced file entry. This creates an ftest_advanced.native executable.
 - `make basic` to compile an algorithm which will accept a basic file entry. This creates an ftest_basic.native executable.
 - `make demo_advanced` to run the `ftest_advanced` program with some arguments
 - `make demo_basic` to run the `ftest_basic` program with some arguments
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).

