#pragma once

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
                                             

DEFVAR(Qt, Vt, "t",
       make_object(ALObject::list_type{Qt}));
DEFVAR(Qnil, Vnil, "nil",
       make_object(ALObject::list_type{Qnil}));

DEFVAR(Qmodpaths, Vmodpaths, "modpaths",
       make_object("", "/home/arnaud/code/alisp/scripts/libs/", ""));

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

}
