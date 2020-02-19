## Builtin Modules

Builtin Modules are built into the Alisp interpreter and can be always imported. These modules are meant to provide common functionality like working with files, basic OS-operations, math functions, etc.

* [fileio](fileio) - The `fileio` moudule provides utilities for working with file paths, files, directories and some basic IO functions.
* [math](math) - The `math` provides more complicated math functions. Often these
function are just wrappers around the standard C++ functions
* [memory](memory) - The `memory` modules provides utilities for working with raw memory
buffers. Memory buffers are just places in memory that are filled with bytes.
* [platform](platform) - The `platform` module exposes infomration about the Alisp
interpreter, the underlying operating system and information about it
as well as how the intrpterter was compiled.
* [system](system) - The `os` modules allows you to access common OS functions through Alisp.
* [time](time) - The `time` module provides utility functions for working with time
and dates.
