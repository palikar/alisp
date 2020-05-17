/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     n the Free Software Foundation; either version 2 of the License, or
     (at your option) any prior version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License along
     with this program; if not, write to the Free Software Foundation, Inc.,
     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#pragma once

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_env.hpp"


namespace alisp
{


/*  _____ _ _            */
/* |  ___(_) | ___  ___  */
/* | |_  | | |/ _ \/ __| */
/* |  _| | | |  __/\__ \ */
/* |_|   |_|_|\___||___/ */

DEFUN(file_open, "file-open", R"((file-open PATH [:out] [:in])

Open a file from the filesystem. If `:out` is specified, the file will
be opened for writing. If `:in` is specified the file will be opened
for reading. Provind both keyword arguemnts is also possible. The
function returns a resrouse descriptor that can be used to access the underlying file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(defvar file-2 (file-open "./file-2.al" :in)
```
)");

DEFUN(file_close, "file-close", R"((file-close FILE)

Close an opened file and release the file descriptor. `FILE` should be
a valid resource descriptor pointing to a file.

```elisp
(defvar file-1 (file-open "./file-1.al" :out)
(file-close file-1)
```
)");

DEFUN(file_read_line, "file-read-line", R"((file-read-line FILE)

Read a single line from a file an return it.`FILE` should be a valid
resource descriptor pointing to a file. This function also moves the
position of the underlying file stream after the read line.

)");

DEFUN(file_write_line, "file-write-line", R"((file-write-line FILE STRING)

Write `STRING` to a file, followed by a new line. `FILE` should be
a valid resource descriptor pointing to a file.

)");

DEFUN(file_has_more, "file-has-more", R"((file-has-more FILE)

Check if there is more data to read of a `FILE`. `FILE` should be a
valid resource descriptor pointing to a file. Return `t` if the stream
pointer has reached to the end of the file and `nil` otherwise.
)");


DEFVAR(Qfiles_all,
       Vfiles_all,
       "--files-all--",
       make_sym_list({ "file-open", "file-close", "file-read-line", "file-write-line", "file-has-more" }),
       R"()");


}  // namespace alisp
