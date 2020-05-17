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

DEFUN(stream, "stream", R"((stream [:from-string STRING] [:from-file FILE]))");

DEFUN(close_stream, "stream-close", R"((stream-close STREAM))");

DEFUN(with_cout, "with-cout", R"((with-cout STREAM))");

DEFUN(with_cin, "with-cin", R"((with-cin STREAM))");

DEFUN(stream_content, "stream-content", R"(((content STREAM))");

DEFUN(stream_write, "stream-write", R"((stream-write VALUE))");

DEFUN(stream_write_line, "stream-write-line", R"((stream-write-line VALUE))");

DEFUN(stream_write_lines, "stream-write-lines", R"((stream-write-line VALUE [[VALUE] ...]))");

DEFUN(stream_read, "stream-read", R"((stream-read))");

DEFUN(stream_read_line, "stream-read-line", R"((stream-read-line))");

DEFUN(stream_read_lines, "stream-read-lines", R"((stream-read-lines))");


}  // namespace alisp
