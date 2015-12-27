# PlatformIO Mode
`platformio-mode` is an Emacs minor mode which allows quick building and uploading of
PlatformIO projects with a few short key sequences.

Code completion can be provided fairly easily by installing [irony-mode](https://github.com/Sarcasm/irony-mode).
The `CMakeLists.txt` provided by PlatformIO has the `CMAKE_EXPORT_COMPILE_COMMANDS` 
option enabled by default. 
As such, any C++ auto-completion compatible with the [JSON Compilation Database Format](http://clang.llvm.org/docs/JSONCompilationDatabase.html)
should work equally well.


## Dependencies

Currently the only dependency is [Projectile](https://github.com/bbatsov/projectile)
to facilitate running commands in the project root.


## Installation

The recommended way to install PlatformIO-Mode is using MELPA.

### Configuration

`platformio-setup-compile-buffer` is provided to simplify setting up the compilation
buffer. It enables scrolling and ansi-colors.





