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

/*   ____                _              _        */
/*  / ___|___  _ __  ___| |_ __ _ _ __ | |_ ___  */
/* | |   / _ \| '_ \/ __| __/ _` | '_ \| __/ __| */
/* | |__| (_) | | | \__ \ || (_| | | | | |_\__ \ */
/*  \____\___/|_| |_|___/\__\__,_|_| |_|\__|___/ */

DEFVAR(Qt, Vt, "t", Qt, R"(A self-evaluating object that represetns "false")");
DEFVAR(Qnil, Vnil, "nil", Qnil, R"(A self-evaluating object that represetns "true")");

DEFVAR(Qmodpaths,
       Vmodpaths,
       "--modpaths--",
       make_object(AL_EXTRA_MODPATHS, AL_EXTRA_MODPATHS),
       R"(A list of strings giving the paths where the interpreter will look when modules are loaded.)");

DEFVAR(Qcurrent_module,
       Vcurrent_module,
       "--module--",
       make_string(""),
       R"(Internal value indicating the current module.)");

DEFVAR(Qcommand_line_args,
       Vcommand_line_args,
       "--argv--",
       make_list(),
       R"(A list of the command line arguements passed to the script.)");

DEFVAR(Qlicense,
       Vlicense,
       "--al-license--",
       make_string(AL_LICENSE),
       R"(A short string signifying the licence for the Alisp interpreter.)");

DEFVAR(
  Qdebug_mode,
  Vdebug_mode,
  "--debug-mode--",
  Qt,
  R"(If non-nil, the interprter has been started in a debug mode. In debug mode, assertions do not raise signal, even if the argument evaluates to something falsey.)");

DEFVAR(Qload_signal,
       Vload_signal,
       "load-signal",
       env::intern("load-signal"),
       R"(Signal raised when a loading error occures)");
DEFVAR(Qdefun_signal,
       Vdefun_signal,
       "defun-signal",
       env::intern("defun-signal"),
       R"(Signal raised when a function definition error occures)");

DEFSYM(Qint, "&int", R"()");
DEFSYM(Qdouble, "&double", R"()");
DEFSYM(Qstring, "&string", R"()");
DEFSYM(Qlist_arg, "&list", R"()");
DEFSYM(Qsym_arg, "&sym", R"()");
DEFSYM(Qchar_arg, "&char", R"()");
DEFSYM(Qnumber_arg, "&number", R"()");
DEFSYM(Qfunction_arg, "&function", R"()");
DEFSYM(Qfile_arg, "&file", R"()");
DEFSYM(Qstream_arg, "&stream", R"()");
DEFSYM(Qmemory_arg, "&memory", R"()");
DEFSYM(Qbyte_arg, "&byte", R"()");
DEFSYM(Qbytearray_arg, "&byte-array", R"()");

DEFSYM(Qoptional, "&optional", R"(Used in an argument list to signify that the next arguments are optional.)");

DEFSYM(Qrest,
       "&rest",
       R"(Used in an argument list to signify that the next arguemnt should be
bound to any left arguments by function call.)");
DEFSYM(Qcomma, ",", R"(Used by the backquote syntax in order to evaluate and expression.)");
DEFSYM(Qcomma_at,
       ",@",
       R"(Used by the backquote syntax in order to slice in a list inside of
another list.)");


DEFVAR(
  Qconstants_all,
  Vconstants_all,
  "--constants-all--",
  make_sym_list({
    "t",
    "nil",
    "--modpaths--",
    "--argv--",
    "--al-license--",
    "--debug-mode--",
  }),
  R"(Alisp has severeal built in variables that cannot be changed and provide certain information about the system.)");


}  // namespace alisp
