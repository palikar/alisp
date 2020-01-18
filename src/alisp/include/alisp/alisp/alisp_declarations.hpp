/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
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
                                             

DEFVAR(Qt, Vt, "t", make_list(Qt));
DEFVAR(Qnil, Vnil, "nil", make_object(Qnil));

DEFVAR(Qmodpaths, Vmodpaths, "--modpaths--", make_object("", "/home/arnaud/code/alisp/scripts/libs/", "/home/arnaud/temp/alisp/scripts/libs/", AL_EXTRA_MODPATHS));
DEFVAR(Qcurrent_module, Vcurrent_module, "--module--", make_string(""));
DEFVAR(Qcommand_line_args, Vcommand_line_args, "--argv--", make_list());

DEFSYM(Qoptional, "&optional");
DEFSYM(Qrest, "&rest");

DEFSYM(Qcomma, ",");
DEFSYM(Qcomma_at, ",@");



// /*  _                                                ____                _                   _        */
// /* | |    __ _ _ __   __ _ _   _  __ _  __ _  ___   / ___|___  _ __  ___| |_ _ __ _   _  ___| |_ ___  */
// /* | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \ | |   / _ \| '_ \/ __| __| '__| | | |/ __| __/ __| */
// /* | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/ | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_\__ \ */
// /* |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___|  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|___/ */
// /*                   |___/             |___/                                                          */


DEFUN(import, "import");
DEFUN(modref, "modref");

DEFUN(defun, "defun");
DEFUN(defmacro, "defmacro");
DEFUN(defvar, "defvar");

DEFUN(setq, "setq");
DEFUN(set, "set");
DEFUN(quote, "quote");
DEFUN(function, "function");
DEFUN(lambda, "lambda");
DEFUN(if, "if");
DEFUN(while, "while");
DEFUN(dolist, "dolist");
DEFUN(cond, "cond");
DEFUN(unless, "unless");
DEFUN(when, "when");
DEFUN(progn, "progn");
DEFUN(letx, "let*");
DEFUN(let, "let");
DEFUN(funcall, "funcall");
DEFUN(backquote, "backquote");
DEFUN(signal, "signal");
DEFUN(return, "return");
DEFUN(exit, "exit");

// /*  ____       _       _   _              */
// /* |  _ \ _ __(_)_ __ | |_(_)_ __   __ _  */
// /* | |_) | '__| | '_ \| __| | '_ \ / _` | */
// /* |  __/| |  | | | | | |_| | | | | (_| | */
// /* |_|   |_|  |_|_| |_|\__|_|_| |_|\__, | */
// /*                                 |___/  */


DEFUN(print, "print");
DEFUN(println, "println");
DEFUN(dump, "dump");
DEFUN(dumpstack, "dumpstack");
DEFUN(dumpcallstack, "dumpcallstack");

/*  _     _     _        */
/* | |   (_)___| |_ ___  */
/* | |   | / __| __/ __| */
/* | |___| \__ \ |_\__ \ */
/* |_____|_|___/\__|___/ */


DEFUN(mapc, "mapc");
DEFUN(car, "car");
DEFUN(cons, "cons");
DEFUN(head, "head");
DEFUN(last, "last");
DEFUN(init, "init");
DEFUN(tail, "tail");
DEFUN(push, "push");
DEFUN(delete, "delete");
DEFUN(remove, "remove");
DEFUN(nth, "nth");
DEFUN(range, "range");
DEFUN(length, "length");


/*  ____               _ _           _             */
/* |  _ \ _ __ ___  __| (_) ___ __ _| |_ ___  ___  */
/* | |_) | '__/ _ \/ _` | |/ __/ _` | __/ _ \/ __| */
/* |  __/| | |  __/ (_| | | (_| (_| | ||  __/\__ \ */
/* |_|   |_|  \___|\__,_|_|\___\__,_|\__\___||___/ */



DEFUN(pfunction, "pfunction");
DEFUN(psym, "psym");
DEFUN(plist, "plist");
DEFUN(pint, "pint");
DEFUN(preal, "preal");
DEFUN(pstring, "pstring");

/*  _                _       */
/* | |    ___   __ _(_) ___  */
/* | |   / _ \ / _` | |/ __| */
/* | |__| (_) | (_| | | (__  */
/* |_____\___/ \__, |_|\___| */
/*             |___/         */

DEFUN(or, "or");
DEFUN(and, "and");
DEFUN(not, "not");


/*  __  __       _   _      */
/* |  \/  | __ _| |_| |__   */
/* | |\/| |/ _` | __| '_ \  */
/* | |  | | (_| | |_| | | | */
/* |_|  |_|\__,_|\__|_| |_| */

DEFUN(plus, "+");
DEFUN(minus, "-");
DEFUN(dev, "/");
DEFUN(multiply, "*");
DEFUN(gt, ">");
DEFUN(geq, ">=");
DEFUN(lt, "<");
DEFUN(leq, "<=");
DEFUN(eq, "==");
DEFUN(neq, "!=");
DEFUN(mod, "mod");
DEFUN(pow, "pow");

DEFUN(leftshift, "<<");
DEFUN(rightshift, ">>");

DEFUN(min, "min");
DEFUN(max, "max");
DEFUN(round, "round");


/*  ____  _        _                  */
/* / ___|| |_ _ __(_)_ __   __ _ ___  */
/* \___ \| __| '__| | '_ \ / _` / __| */
/*  ___) | |_| |  | | | | | (_| \__ \ */
/* |____/ \__|_|  |_|_| |_|\__, |___/ */
/*                         |___/      */

DEFUN(string_contains, "string-contains");
DEFUN(string_endswith, "string-endswith");
DEFUN(string_startswith, "string-startswith");
DEFUN(string_length, "string-length");
DEFUN(string_capitalize, "string-capitalize");
DEFUN(string_find, "string-find");
DEFUN(string_replace, "string-replace");
DEFUN(string_replaceall, "string-replaceall");
DEFUN(string_split, "string-split");
DEFUN(string_substring, "string-substring");
DEFUN(string_splitlines, "string-splitlines");
DEFUN(string_upper, "string-upper");
DEFUN(string_lower, "string-lower");
DEFUN(string_strip, "string-strip");
DEFUN(string_join, "string-join");

DEFUN(char_isalpha, "char-isalpha");
DEFUN(char_isdigit, "char-isdigit");


/*     _    _                  _ _   _                    */
/*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  ___  */
/*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \/ __| */
/*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | \__ \ */
/* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|___/ */
/*            |___/                                       */


DEFUN(slice, "slice");
DEFUN(sort, "sort");
DEFUN(zip, "zip");
DEFUN(filter, "filter");
DEFUN(any, "any");
DEFUN(all, "all");

/*  ____                      */
/* |  _ \ __ _ _ __ ___  ___  */
/* | |_) / _` | '__/ __|/ _ \ */
/* |  __/ (_| | |  \__ \  __/ */
/* |_|   \__,_|_|  |___/\___| */


DEFUN(int_parse, "parse-int");
DEFUN(float_parse, "parse-float");
DEFUN(to_string, "to-string");
DEFUN(to_char, "to-char");



}
