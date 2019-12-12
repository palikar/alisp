#pragma once

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_env.hpp"


namespace alisp
{


/*   ____                _              _        */
/*  / ___|___  _ __  ___| |_ __ _ _ __ | |_ ___  */
/* | |   / _ \| '_ \/ __| __/ _` | '_ \| __/ __| */
/* | |__| (_) | | | \__ \ || (_| | | | | |_\__ \ */
/*  \____\___/|_| |_|___/\__\__,_|_| |_|\__|___/ */
                                             

DEFVAR(Qt, "t");
DEFVAR(Qnil, "nil");

DEFSYM(Qoptional, "&optional");
DEFSYM(Qrest, "&rest");


/*  _                                                ____                _                   _        */
/* | |    __ _ _ __   __ _ _   _  __ _  __ _  ___   / ___|___  _ __  ___| |_ _ __ _   _  ___| |_ ___  */
/* | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \ | |   / _ \| '_ \/ __| __| '__| | | |/ __| __/ __| */
/* | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/ | |__| (_) | | | \__ \ |_| |  | |_| | (__| |_\__ \ */
/* |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___|  \____\___/|_| |_|___/\__|_|   \__,_|\___|\__|___/ */
/*                   |___/             |___/                                                          */


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

DEFUN(signal, "signal");
DEFUN(exit, "exit");

/*  ____       _       _   _              */
/* |  _ \ _ __(_)_ __ | |_(_)_ __   __ _  */
/* | |_) | '__| | '_ \| __| | '_ \ / _` | */
/* |  __/| |  | | | | | |_| | | | | (_| | */
/* |_|   |_|  |_|_| |_|\__|_|_| |_|\__, | */
/*                                 |___/  */


DEFUN(print, "print");
DEFUN(println, "println");
DEFUN(dump, "dump");

/*  _     _     _        */
/* | |   (_)___| |_ ___  */
/* | |   | / __| __/ __| */
/* | |___| \__ \ |_\__ \ */
/* |_____|_|___/\__|___/ */


DEFUN(mapc, "mapc");



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

}

