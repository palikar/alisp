# File system.

Similar to stream, files are also resource objects. From a programmer perspecive a file is accesed through a poineter line integer object. As in other languages, files can be opend, closed, written to and read from.

## Opening and closing files

Opening and closing a file is done through these two simple to use functions.

!!! Warning
	Every file that was opened must be closed. If a file is not closed properly, memory could be leaked.

- **file-open** : *(file-open PATH [:out] [:in])*

Open a file from the filesystem. If `:out` is specified, the file will
be opened for writing. If `:in` is specified the file will be opened
for reading. Provind both keyword arguemnts is also possible. The
function returns a resrouse descriptor that can be used to access the underlying file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(defvar file-2 (file-open "./file-2.al" :in)
```


- **file-close** : *(file-close FILE)*

Close an opened file and release the file descriptor. `FILE` should be
a valid resource descriptor pointing to a file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(file-close file-1)
```


## Funcitons for basic reading from and writing to files.



!!! Tip
	If you need more reading and writing functions, attach a stram to the file and work the the stream itself.

- **file-read-line** : *(file-read-line FILE)*

Read a single line from a file an return it.`FILE` should be a valid
resource descriptor pointing to a file. This function also moves the
position of the underlying file stream after the read line.



- **file-write-line** : *(file-write-line FILE STRING)*

Write `STRING` to a file, followed by a new line. `FILE` should be
a valid resource descriptor pointing to a file.



- **file-has-more** : *(file-has-more FILE)*

Check if there is more data to read of a `FILE`. `FILE` should be a
valid resource descriptor pointing to a file. Return `t` if the stream
pointer has reached to the end of the file and `nil` otherwise.


