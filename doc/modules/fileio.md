### Filio

#### Description

The `fileio` moudule provides utilities for working with file paths,
files, directories and some basic IO functions.

#### Functions

**f-empty** : *(f-empty PATH)*

If `PATH` is a file, return `t` if the file in `PATH` is empty, `nil`
otherwise. If `PATH` is directory, return `t` if directory has no files,
`nil` otherwise.


**f-descendant-of** : *(f-descendant-of PATH)*

Return `t` if `PATH1` is desendant of `PATH2`.


**f-child-of** : *(f-child-of PATH1 PATH2)*

Return t if `PATH1` is child of `PATH2`.


**f-absolute** : *(f-absolute PATH)*

Return `t` if `PATH` is absolute, `nil` otherwise.


**f-executable** : *(f-executable PATH)*

Return `t` if `PATH` is executable, `nil` otherwise.


**f-writable** : *(f-writable PATH)*

Return `t` if `PATH` is writable, `nil` otherwise.


**f-symlink** : *(f-symlink PATH)*

Return `t` if `PATH` is symlink, `nil` otherwise.


**f-file** : *(f-file PATH)*

Return `t` if `PATH` is `nil`, false otherwise.


**f-exists** : *(f-exists PATH)*

Return `t` if `PATH` exists, `nil` otherwise.


**f-parent-of** : *(f-parent-of PATH1 PATH2)*

Return t if `PATH1` is parent of `PATH2`.


**f-long** : *(f-long PATH)*

Return long version of `PATH`.


**f-short** : *(f-short PATH)*

Return abbrev of `PATH`.


**f-hidden** : *(f-hidden PATH)*

Return `t` if `PATH` is hidden, `nil` otherwise.


**f-readable** : *(f-readable PATH)*

Return `t` if `PATH` is readable, `nil` otherwise.


**f-relative** : *(f-relative PATH)*



**f-base** : *(f-base PATH)*

Return the name of `PATH`, excluding the extension of file.


**f-swap-ext** : *(f-swap-ext PATH)*

Return the file extension of `PATH`. The extension, in a file name, is
the part that follows the last ’.’, excluding version numbers and
backup suffixes.


**f-write-bytes** : *(f-write-bytes PATH BYTES)*

Write the bytes `BYTES` to the file pointed by `PATH`. Previous content is erased.


**f-move** : *(f-move FROM TO)*

Move or rename `FROM` to `TO`.


**f-join** : *(f-join [ARGS] ...)*

Join `ARGS` to a single path.


**f-prelative** : *(f-prelative PATH)*

Return `t` if `PATH` is relative, `nil` otherwise.


**f-expand-user** : *(f-expand-user PATH)*

For unix systems, expand `~` to the location of the home directory of
the current user.


**f-write-text** : *(f-write-text PATH TEXT)*

Write `TEXT` to the file pointed by `PATH`. Previous content is erased.


**f-append-bytes** : *(f-append-bytes PATH BYTES)*

Append the bytes `BYTES` to the file pointed by `PATH`. This function does not
erase the prevous contents of the file.


**f-touch** : *(f-touch PATH)*

Update `PATH` last modification date or create if it does not exist.


**f-read-text** : *(f-read-text PATH)*

Read the text from the file `PATH` and return the contatns as a string.


**f-entries** : *(f-entries PATH)*

Find all files and directories in `PATH`.


**f-copy** : *(f-copy FROM TO)*

Copy file or directory `FROM` to `TO`.


**f-filename** : *(f-filename PATH)*

Return the name of `PATH`.


**f-temp-file** : *(f-temp-file PATH)*

Return a resource object ot a temporary file. The file is created and
the object can be used for writing to the file.


**f-temp-file-name** : *(f-temp-file-name PATH)*

Return a path to a temporary file. The file is not created but the
path will be valid for a temporary file.


**f-full** : *(f-full PATH)*

Return absolute path to `PATH`, with ending slash.


**f-with-temp-file** : *(f-with-temp-file FILE-SYM BODY)*

Bind `FILE-SYM` and execute the forms in `BODY`. `FILE-SYM` will point
to a valid file resource of a temporary file.


**f-same** : *(f-same PATH1 PATH2)*

Return `t` if `PATH1` and `PATH2` are references to same file.


**f-common-parent** : *(f-common-parent [PATHS] ...)*

Return the deepest common parent directory of `PATHS`.


**f-ancestor-of** : *(f-ancestor-of PATH1 PATH2)*

Return `t` if `PATH1` is ancestor of `PATH2`.


**f-directories** : *(f-directories PATH)*

Find all directories in `PATH`.


**f-direcotry** : *(f-direcotry PATH)*

Return `t` if `PATH` is directory, `nil` otherwise.


**f-make-symlink** : *(f-make-symlink SOURCE PATH)*

Create a symlink to `SOURCE` from `PATH`.


**f-is-root** : *(f-is-root PATH)*

Return `t` if `PATH` is root directory, `nil` otherwise.


**f-delete** : *(f-delete PATH)*

Delete `PATH`, which can be file or directory.


**f-mkdir** : *(f-mkdir DIR)*

Create the directory `DIR`.


**f-canonical** : *(f-canonical PATH)*

Return the canonical name of `PATH`.


**f-ext** : *(f-ext PATH)*



**f-no-ext** : *(f-no-ext PATH)*



**f-root** : *(f-root)*

Return absolute root.


**f-glob** : *(f-glob PATTERN PATH)*

Find `PATTERN` in `PATH`.


**f-read-bytes** : *(f-read-bytes PATH)*

Read binary data from `PATH`. Return the binary data as byte array.


**f-append-text** : *(f-append-text PATH TEXT)*

Append `TEXT` to the file pointed by `PATH`. This function does not
erase the prevous contents of the file.  

**f-split** : *(f-split PATH)*

Split `PATH` and return list containing parts.


**f-expand** : *(f-expand PATH DIR)*

Expand `PATH` relative to `DIR`.


**f-dirname** : *(f-dirname PATH)*

Return the parent directory to `PATH`.


#### Constants


**f-directory-separator** : String containing the native symbol to separate directories and files
in a path. On unix systems this is forward-slash and on Windows
backslash.

