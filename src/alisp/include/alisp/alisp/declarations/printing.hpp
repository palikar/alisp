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


// /*  ____       _       _   _              */
// /* |  _ \ _ __(_)_ __ | |_(_)_ __   __ _  */
// /* | |_) | '__| | '_ \| __| | '_ \ / _` | */
// /* |  __/| |  | | | | | |_| | | | | (_| | */
// /* |_|   |_|  |_|_| |_|\__|_|_| |_|\__, | */
// /*                                 |___/  */


DEFUN(print, "print", R"((print FORM [[FORM] ...])

Print the value of VALUE of form on the standard output stream.
)");

DEFUN(println, "println", R"((println VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard output stream and put a new
line character.
)");

DEFUN(eprint, "eprint", R"((eprint VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard error stream.
)");

DEFUN(eprintln, "eprintln", R"((eprintln VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard error stream and put a new
line character.
)");


DEFUN(dump, "dump", R"((dump FORM))");
DEFUN(dumpstack, "dumpstack", R"((dumpstack)

Print a formatted version of the current state of the execution
environment. This is where the stack frames and the scopes live.
)");

DEFUN(dumpcallstack, "dumpcallstack", R"((dumpcallstack)

Print a formatted version of the current call stack on the standard
output. This function is meant for debugging.
)");

DEFUN(dumpsystem, "dumpsystem", R"((dumpsystem)

Print out the currenttly bounded symbols in the interpreter. This
function is meant for debugging.
)");

DEFUN(dumplicense, "dumplicense", R"((dumplicense)

Print the license of Alisp on the standard output.
)");

DEFUN(dumpcredits, "dumpcredits", R"((dumpcredits)

Print the contributors information for Alisp on the standard output.
)");

DEFUN(dumpbuildscript, "dumpbuildscript", R"((dumpbuildscript)

Print the string that gets printed out on start of the repl interpreter.
)");

DEFUN(read_line, "read-line", R"((read-line)

Read a single line form the standard input stream and return it.
)");

DEFUN(read_char, "read-char", R"((read-char)

Read a single character form the standard input stream and return it.
)");


DEFVAR(
  Qlists_all,
  Vlists_all,
  "--lists-all--",
  make_sym_list(
    { "length", "cons", "head", "last", "init", "tail", "nth", "mapc", "mapcar", "push", "delete", "remove", "range" }),
  R"(Functions to access the elements of lists as well to perform some basic modifications on lists.)");

}  // namespace alisp
