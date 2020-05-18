# Streaming system.

Alisp provides a mechanism for working with streams. Streams are abstraction that supports writing and reading and provide unified interface for these operation. In ALisp streams are handled through *resource objects*. This means that every strema is identified through a int-object that acts like a pointer to the underlying stream. The intrpterer keeps track of every opened stream and provides acces to each of them throught the resource object (the int value).

## Opening and closing streams

These functions are used to open and close a stream.

!!! Warning
	Every stream that was opened must be closed. If a stream is not closed, memory could be leaked.

- **stream** : *(stream [:from-string STRING] [:from-file FILE])*

Open a stream that can be used with the other stream writing and
reading functions. If `:from-string` is gliven, the stream will be
writing\reading to\from the given string. If `:from-file` is given,
the stream will write\read to\from the file. The file must be opened
in the appropriate mode.

Return the newly created stream as alisp-resource.


- **stream-close** : *(stream-close STREAM)*

Close the stream `STREAM`. `STREAM` has to be alisp-resource that was
taken from previous call to `stream`.



## Writing.

Functions for writing to streams


- **stream-write** : *(stream-write STREAM VALUE)*

Write the value `VALUE` the stream `STREAM`.


- **stream-write-line** : *(stream-write-line STREAM VALUE)*

Write the line `VALUE` the stream `STREAM`.


- **stream-write-lines** : *(stream-write-lines STREAM [[VALUE] ...])*

Write the line `VALUE` the stream `STREAM`.


## Reading.

Functions for reading from streams


- **stream-read** : *(stream-read STREAM)*

Read the next available character in the stream `STREAM`.


- **stream-read-line** : *(stream-read-line STREAM)*

Read the next available line(string ending with \n) in the stream `STREAM`.


- **stream-read-lines** : *(stream-read-lines STREAM)*

Read all of the available lines in the stream `STREAM`.


## Redirecting standrad output and input.

The sandard input and output streams of the process can be redirected from any compatable stream. This way, you can use functions that will normaly pring to the standard output, but have the stream redirected to a file for example.


- **with-cout** : *(with-cout STREAM BODY)*

Rebind the standard output to the stream `STREAM` and execute the
forms in `BODY`.

Example:
```elisp
(with-cout (stream :from-file (file-open "out.txt" :out))
   (println "this goes to the file")
)
```


- **with-cin** : *(with-cin STREAM BODY)*

Rebind the standard input to the stream `STREAM` and execute the forms
in `BODY`. When functions that read form the standard input are used,
they'll read from the given stream instead.



## Utilities


Some utility functions for working with streams

- **stream-content** : *((content STREAM)*

Return the content of the stream `STREAM` as a string.


