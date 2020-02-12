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

DEFVAR(Qmodpaths,
       Vmodpaths,
       "--modpaths--",
       make_object("", "/home/arnaud/code/alisp/scripts/libs/", "/home/arnaud/temp/alisp/scripts/libs/", AL_EXTRA_MODPATHS));
DEFVAR(Qcurrent_module, Vcurrent_module, "--module--", make_string(""));
DEFVAR(Qcommand_line_args, Vcommand_line_args, "--argv--", make_list());

DEFVAR(Qlicense, Vlicense, "--al-license--", make_string(AL_LICENSE));

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


DEFUN(import, "import", R"((import NAME [:file file] [:all] [( [(SYM MAPPED)]... )]))");
DEFUN(modref, "modref", R"((modref MODUE [[MODUE] ...] SYMBOL [[symbol] ...] ))");


DEFUN(defun, "defun", R"((defun NAME (ARGLIST) [DOC] BODY))");
DEFUN(defmacro, "defmacro", R"((defmacro NAME (ARGLIST) [DOC] BODY))");
DEFUN(defvar, "defvar", R"((defvar NAME VALUE [DOC]))");

DEFUN(eval, "eval", R"((eval FORM))");
DEFUN(setq, "setq", R"((setq SYMBOL VALUE))");
DEFUN(set, "set", R"(((set SYMBOL VALUE)))");
DEFUN(quote, "quote", R"((quote OBJECT))");
DEFUN(function, "function", R"((funttion OBJECT))");
DEFUN(lambda, "lambda", R"((lambda (ARGLIST) BODY))");
DEFUN(if, "if", R"((if CONDITION THEN ELSE))");
DEFUN(while, "while", R"((while CONDITION BODY))");
DEFUN(dolist, "dolist", R"((dolist (SYMBOL LIST) BODY))");
DEFUN(cond, "cond", R"((cond [[CODITION BODY] ... ]))");
DEFUN(unless, "unless", R"((unless CONDITION BODY))");
DEFUN(when, "when", R"((when CONDITION BODY))");
DEFUN(progn, "progn", R"((progn BODY))");
DEFUN(letx, "let*", R"((let* ([[VAR]...] [[(VAR VALUE)] ...] ) BODY))");
DEFUN(let, "let", R"((let ([[VAR]...] [[(VAR VALUE)] ...] ) BODY))");
DEFUN(funcall, "funcall", R"((funcall SYMBOL LIST))");
DEFUN(backquote, "backquote", R"((`LIST))");
DEFUN(signal, "signal", R"((signal SYMBOL LIST))");
DEFUN(return, "return", R"((return [VALUE]))");
DEFUN(exit, "exit", R"((exit [VALUE]))");

// /*  ____       _       _   _              */
// /* |  _ \ _ __(_)_ __ | |_(_)_ __   __ _  */
// /* | |_) | '__| | '_ \| __| | '_ \ / _` | */
// /* |  __/| |  | | | | | |_| | | | | (_| | */
// /* |_|   |_|  |_|_| |_|\__|_|_| |_|\__, | */
// /*                                 |___/  */


DEFUN(print, "print", R"((print VALUE [[VALUE] ...]))");
DEFUN(println, "println", R"((println VALUE [[VALUE] ...]))");
DEFUN(eprint, "eprint", R"((eprint VALUE [[VALUE] ...]))");
DEFUN(eprintln, "eprintln", R"((eprintln VALUE [[VALUE] ...]))");

DEFUN(dump, "dump", R"(())");
DEFUN(dumpstack, "dumpstack", R"(())");
DEFUN(dumpcallstack, "dumpcallstack", R"(())");

DEFUN(dumplicense, "dumplicense", R"((dumplicense))");
DEFUN(dumpcredits, "dumpcredits", R"((dumpcredits))");

DEFUN(read_line, "read-line", R"((read-line))");


/*  _     _     _        */
/* | |   (_)___| |_ ___  */
/* | |   | / __| __/ __| */
/* | |___| \__ \ |_\__ \ */
/* |_____|_|___/\__|___/ */


DEFUN(mapc, "mapc", R"((mapc FUNCTION LIST))");
DEFUN(mapcar, "mapcar", R"((mapcar FUNCTION LIST))");
DEFUN(car, "car", R"((mapc LIST))");
DEFUN(cons, "cons", R"((cons LIST))");
DEFUN(head, "head", R"((head LIST))");
DEFUN(last, "last", R"((last LIST))");
DEFUN(init, "init", R"((init LIST))");
DEFUN(tail, "tail", R"((tail LIST))");
DEFUN(push, "push", R"((push LIST ELEMENT))");
DEFUN(delete, "delete", R"((delete LIST ELEMENT))");
DEFUN(remove, "remove", R"((remove LIST ELEMENT))");
DEFUN(nth, "nth", R"((nth LIST INDEX))");
DEFUN(range, "range", R"((range FROM TO))");
DEFUN(length, "length", R"((length LIST))");


/*  ____  _                                 */
/* / ___|| |_ _ __ ___  __ _ _ __ ___  ___  */
/* \___ \| __| '__/ _ \/ _` | '_ ` _ \/ __| */
/*  ___) | |_| | |  __/ (_| | | | | | \__ \ */
/* |____/ \__|_|  \___|\__,_|_| |_| |_|___/ */

DEFUN(stream, "stream", R"((stream [:from-string STRING] [:from-file FILE]))");
DEFUN(close_stream, "stream-close", R"((stream-close STREAM))");
// DEFUN(with_stream, "with-streaRm(())");
DEFUN(with_cout, "with-cout", R"((with-cout STREAM))");
DEFUN(with_cin, "with-cin", R"((with-cin STREAM))");

DEFUN(stream_content, "stream-content", R"(((content STRREAM))");

DEFUN(stream_write, "stream-write", R"((stream-write VALUE))");
DEFUN(stream_write_line, "stream-write-line", R"((stream-write-line VALUE))");
DEFUN(stream_write_lines, "stream-write-lines", R"((stream-write-line VALUE [[VALUE] ...]))");
DEFUN(stream_read, "stream-read", R"((stream-read))");
DEFUN(stream_read_line, "stream-read-line", R"((stream-read-line))");
DEFUN(stream_read_lines, "stream-read-lines", R"((stream-read-lines))");


/*  _____ _ _            */
/* |  ___(_) | ___  ___  */
/* | |_  | | |/ _ \/ __| */
/* |  _| | | |  __/\__ \ */
/* |_|   |_|_|\___||___/ */

DEFUN(file_open, "file-open", R"((file-open PATH [:out] [:in]))");
DEFUN(file_close, "file-close", R"((file-close FILE))");
DEFUN(file_read_line, "file-read-line", R"((file-read-line FILE VALUE))");
DEFUN(file_write_line, "file-write-line", R"((file-write-line FILE))");
DEFUN(file_has_more, "file-has-more", R"((file-has-more))");


/*  ____                       */
/* |  _ \ _ __ ___  _ __  ___  */
/* | |_) | '__/ _ \| '_ \/ __| */
/* |  __/| | | (_) | |_) \__ \ */
/* |_|   |_|  \___/| .__/|___/ */
/*                 |_| */


DEFUN(prop_get, "prop-get", R"((prop-get SYM PROPERTY))");
DEFUN(prop_set, "prop-set", R"((prop-set SYM PROPERTY VALUE))");
DEFUN(prop_list, "prop-list", R"((prop-list SYM))");


/*  ____               _ _           _             */
/* |  _ \ _ __ ___  __| (_) ___ __ _| |_ ___  ___  */
/* | |_) | '__/ _ \/ _` | |/ __/ _` | __/ _ \/ __| */
/* |  __/| | |  __/ (_| | | (_| (_| | ||  __/\__ \ */
/* |_|   |_|  \___|\__,_|_|\___\__,_|\__\___||___/ */


DEFUN(pfunction, "pfunction", R"((pfunction SYMBOL))");
DEFUN(psym, "psym", R"((psym SYMBOL))");
DEFUN(plist, "plist", R"((plist SYMBOL))");
DEFUN(pint, "pint", R"((pint SYMBOL))");
DEFUN(preal, "preal", R"((preal SYMBOL))");
DEFUN(pstring, "pstring", R"((pstring SYMBOL))");

/*  _                _       */
/* | |    ___   __ _(_) ___  */
/* | |   / _ \ / _` | |/ __| */
/* | |__| (_) | (_| | | (__  */
/* |_____\___/ \__, |_|\___| */
/*             |___/         */

DEFUN(or, "or", R"((or [[VALUE]...]))");
DEFUN(and, "and", R"((and [[VALUE]...]))");
DEFUN(not, "not", R"((not VALUE))");


/*  __  __       _   _      */
/* |  \/  | __ _| |_| |__   */
/* | |\/| |/ _` | __| '_ \  */
/* | |  | | (_| | |_| | | | */
/* |_|  |_|\__,_|\__|_| |_| */

DEFUN(plus, "+", R"((+ [[VALUE]...]))");
DEFUN(minus, "-", R"((- [[VALUE]...]))");
DEFUN(dev, "/", R"((/ [[VALUE]...]))");
DEFUN(multiply, "*", R"((* [[VALUE]...]))");
DEFUN(gt, ">", R"((> VALUE1 VALUE2))");
DEFUN(geq, ">=", R"((>= VALUE1 VALUE2))");
DEFUN(lt, "<", R"((< VALUE1 VALUE2))");
DEFUN(leq, "<=", R"((<= VALUE1 VALUE2))");
DEFUN(eq, "==", R"((== VALUE1 VALUE2))");
DEFUN(neq, "!=", R"((!= VALUE1 VALUE2))");
DEFUN(mod, "mod", R"((mod VALUE1))");
DEFUN(pow, "pow", R"((pow VALUE1 VALUE2))");

DEFUN(leftshift, "<<", R"((<< VALUE))");
DEFUN(rightshift, ">>", R"((>> VALUE))");

DEFUN(min, "min", R"((min [[VALUE]...]))");
DEFUN(max, "max", R"((max [[VALUE]...]))");
DEFUN(round, "round", R"((round VALUE PLACES))");


/*  ____  _        _                  */
/* / ___|| |_ _ __(_)_ __   __ _ ___  */
/* \___ \| __| '__| | '_ \ / _` / __| */
/*  ___) | |_| |  | | | | | (_| \__ \ */
/* |____/ \__|_|  |_|_| |_|\__, |___/ */
/*                         |___/      */

DEFUN(string_contains, "string-contains", R"((string-contains STRING SUBSTRING))");
DEFUN(string_endswith, "string-endswith", R"((string-contains STRING SUFFIX))");
DEFUN(string_startswith, "string-startswith", R"((string-contains STRING PREFIX))");
DEFUN(string_length, "string-length", R"((string-length STRING))");
DEFUN(string_capitalize, "string-capitalize", R"((string-capitalize STRING))");
DEFUN(string_find, "string-find", R"((string-find STRING SUBSTRING))");
DEFUN(string_replace, "string-replace", R"((string-replace STRING SUBSTRING NEWSTRING))");
DEFUN(string_replaceall, "string-replaceall", R"((string-replaceall STRING SUBSTRING NEWSTRING))");
DEFUN(string_split, "string-split", R"((string-split STRING DELIMETER))");
DEFUN(string_substring, "string-substring", R"((string-substring STRING FROM TO))");
DEFUN(string_splitlines, "string-splitlines", R"((string-splitlines STRING))");
DEFUN(string_upper, "string-upper", R"((string-upper STRING))");
DEFUN(string_lower, "string-lower", R"((string-lower STRING))");
DEFUN(string_strip, "string-strip", R"((string-strip STRING))");
DEFUN(string_join, "string-join", R"((string-join STRING [[STRING] ...]))");

DEFUN(char_isalpha, "char-isalpha", R"((char-isalpha CHAR))");
DEFUN(char_isdigit, "char-isdigit", R"((char-isdigit CHAR))");


/*     _    _                  _ _   _                    */
/*    / \  | | __ _  ___  _ __(_) |_| |__  _ __ ___  ___  */
/*   / _ \ | |/ _` |/ _ \| '__| | __| '_ \| '_ ` _ \/ __| */
/*  / ___ \| | (_| | (_) | |  | | |_| | | | | | | | \__ \ */
/* /_/   \_\_|\__, |\___/|_|  |_|\__|_| |_|_| |_| |_|___/ */
/*            |___/                                       */


DEFUN(slice, "slice", R"((slice LIST FROM TO))");
DEFUN(sort, "sort", R"((sort LIST))");
DEFUN(zip, "zip", R"((zip [[LIST] ...]))");
DEFUN(filter, "filter", R"((filter FUNCTION LIST))");
DEFUN(any, "any", R"((any FUNCTION LIST))");
DEFUN(all, "all", R"((all FUNCTION LIST))");

/*  ____                      */
/* |  _ \ __ _ _ __ ___  ___  */
/* | |_) / _` | '__/ __|/ _ \ */
/* |  __/ (_| | |  \__ \  __/ */
/* |_|   \__,_|_|  |___/\___| */


DEFUN(int_parse, "parse-int", R"((parse-int STRING))");
DEFUN(float_parse, "parse-float", R"((parse-float STRING))");
DEFUN(to_string, "to-string", R"((to-string VALUE))");
DEFUN(to_char, "to-char", R"((to-char INT))");


}  // namespace alisp
