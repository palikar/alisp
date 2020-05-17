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

DEFVAR(Qt, Vt, "t", Qt, R"()");
DEFVAR(Qnil, Vnil, "nil", Qnil, R"()");

DEFVAR(Qmodpaths, Vmodpaths, "--modpaths--", make_object(AL_EXTRA_MODPATHS, AL_EXTRA_MODPATHS), R"()");
DEFVAR(Qcurrent_module, Vcurrent_module, "--module--", make_string(""), R"()");
DEFVAR(Qcommand_line_args, Vcommand_line_args, "--argv--", make_list(), R"()");
DEFVAR(Qlicense, Vlicense, "--al-license--", make_string(AL_LICENSE), R"()");
DEFVAR(Qdebug_mode, Vdebug_mode, "--debug-mode--", Qt, R"()");

DEFVAR(Qload_signal, Vload_signal, "load-signal", env::intern("load-signal"), R"()");
DEFVAR(Qdefun_signal, Vdefun_signal, "defun-signal", env::intern("defun-signal"), R"()");

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

}  // namespace alisp
