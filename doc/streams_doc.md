# Streaming system.

Alisp provides a mechanism for working with streams. Streams are abstraction that supports writing and reading and provide unified interface for these operation. In ALisp streams are handled through *resource objects*. This means that every strema is identified through a int-object that acts like a pointer to the underlying stream. The intrpterer keeps track of every opened stream and provides acces to each of them throught the resource object (the int value).

## Opening and closing streams

These functions are used to open and close a stream.

!!! Warning
	Every stream that was opened must be closed. If a stream is not closed, memory could be leaked.

> ##### **stream** : *(stream [:from-string STRING] [:from-file FILE])*

> ##### **stream-close** : *(stream-close STREAM)*

## Writing.

Functions for writing to streams


> ##### **stream-write** : *(stream-write VALUE)*

> ##### **stream-write-line** : *(stream-write-line VALUE)*

> ##### **stream-write-lines** : *(stream-write-line VALUE [[VALUE] ...])*

## Reading.

Functions for reading from streams


> ##### **stream-read** : *(stream-read)*

> ##### **stream-read-line** : *(stream-read-line)*

> ##### **stream-read-lines** : *(stream-read-lines)*

## Redirecting standrad output and input.

The sandard input and output streams of the process can be redirected from any compatable stream. This way, you can use functions that will normaly pring to the standard output, but have the stream redirected to a file for example.


> ##### **with-cout** : *(with-cout STREAM)*

> ##### **with-cin** : *(with-cin STREAM)*

## Utilities


Some utility functions for working with streams

> ##### **stream-content** : *((content STRREAM)*

