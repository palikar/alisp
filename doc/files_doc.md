# File system.

Similar to stream, files are also resource objects. From a programmer perspecive a file is accesed through a poineter line integer object. As in other languages, files can be opend, closed, written to and read from.

## Opening and closing files

Opening and closing a file is done through these two simple to use functions.

!!! Warning
	Every file that was opened must be closed. If a file is not closed properly, memory could be leaked.

#### **file-open**: *(file-open PATH [:out] [:in])*

#### **file-close**: *(file-close FILE)*

## Funcitons for basic reading from and writing to files.



!!! Tip
	If you need more reading and writing functions, attach a stram to the file and work the the stream itself.

#### **file-read-line**: *(file-read-line FILE VALUE)*

#### **file-write-line**: *(file-write-line FILE)*

#### **file-has-more**: *(file-has-more)*

