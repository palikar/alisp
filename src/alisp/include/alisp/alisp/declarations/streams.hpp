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


/*  ____  _                                 */
/* / ___|| |_ _ __ ___  __ _ _ __ ___  ___  */
/* \___ \| __| '__/ _ \/ _` | '_ ` _ \/ __| */
/*  ___) | |_| | |  __/ (_| | | | | | \__ \ */
/* |____/ \__|_|  \___|\__,_|_| |_| |_|___/ */

DEFUN(stream, "stream", R"((stream [:from-string STRING] [:from-file FILE])

Open a stream that can be used with the other stream writing and
reading functions. If `:from-string` is gliven, the stream will be
writing\reading to\from the given string. If `:from-file` is given,
the stream will write\read to\from the file. The file must be opened
in the appropriate mode.

Return the newly created stream as alisp-resource.
)");

DEFUN(close_stream, "stream-close", R"((stream-close STREAM)

Close the stream `STREAM`. `STREAM` has to be alisp-resource that was
taken from previous call to `stream`.

)");

DEFUN(with_cout, "with-cout", R"((with-cout STREAM BODY)

Rebind the standard output to the stream `STREAM` and execute the
forms in `BODY`.

Example:
```elisp
(with-cout (stream :from-file (file-open "out.txt" :out))
   (println "this goes to the file")
)
```
)");

DEFUN(with_cin, "with-cin", R"((with-cin STREAM BODY)

Rebind the standard input to the stream `STREAM` and execute the forms
in `BODY`. When functions that read form the standard input are used,
they'll read from the given stream instead.

)");

DEFUN(stream_content, "stream-content", R"(((content STREAM)

Return the content of the stream `STREAM` as a string.
)");

DEFUN(stream_write, "stream-write", R"((stream-write STREAM VALUE)

Write the value `VALUE` the stream `STREAM`.
)");

DEFUN(stream_write_line, "stream-write-line", R"((stream-write-line STREAM VALUE)

Write the line `VALUE` the stream `STREAM`.
)");

DEFUN(stream_write_lines, "stream-write-lines", R"((stream-write-lines STREAM [[VALUE] ...])

Write the line `VALUE` the stream `STREAM`.
)");

DEFUN(stream_read, "stream-read", R"((stream-read STREAM)

Read the next available character in the stream `STREAM`.
)");

DEFUN(stream_read_line, "stream-read-line", R"((stream-read-line STREAM)

Read the next available line(string ending with \n) in the stream `STREAM`.
)");

DEFUN(stream_read_lines, "stream-read-lines", R"((stream-read-lines STREAM)

Read all of the available lines in the stream `STREAM`.
)");


}  // namespace alisp
