### Memory

#### Description

The `memory` modules provides utilities for working with raw memory
buffers. Memory buffers are just places in memory that are filled with bytes.

#### Functions

**buffer-get** : *(buffer-get BUFFER)*

Return the contents of a buffer as a byte array.


**buffer-fill** : *(buffer-fill BUFFER VALUE)*

Fill the entirety of a buffer with `VALUE`.


**buffer-range-get** : *(buffer-range-get BUFFER START INDEX)*

Return part of a buffer as byte array. The returned bytes are in the
range [`START`, `INDEX`)


**buffer-nth-set** : *(buffer-nth-set BUFFER INDEX VALUE)*

Set the value of the `BUFFER` at the given index to `VALUE`.


**buffer-nth-get** : *(buffer-nth-get BUFFER INDEX)*

Return the value of the `BUFFER` at the given index.


**buffer-size** : *(buffer-get-size BUFFER)*

Return the size of the given buffer.


**buffer-release** : *(buffer-release BUFFER)*

Deallocate `BUFFER` (resource object) and free the used memory.


**buffer-set** : *(buffer-set BUFFER BYTE-ARRAY)*

Set the contents of a `BUFFER` to the values in the given byte array.


**buffer-allocate** : *(buffer-allocate SIZE)*

Allocate a buffer of size `SIZE` and return a resource object for the
newly created buffer. The buffer can then be used with other function
for reading and writing bytes to it.


**buffer-mmap** : *(buffer-mmap BUFFER-SOURCE BUFFER-DEST SIZE)*

Copy `SIZE` bytes of `BUFFER-SOURCE` to `BUFFER-DEST`.


#### Constants


