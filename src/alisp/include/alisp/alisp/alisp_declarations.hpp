
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_env.hpp"


namespace alisp
{


DEFVAR(Qt, "t");
DEFVAR(Qnil, "nil");

DEFSYM(Qoptional, "&optional");
DEFSYM(Qrest, "&rest");

DEFUN(defun, "defun");
DEFUN(defmacro, "defmacro");
DEFUN(defvar, "defvar");

DEFUN(signal, "signal");

DEFUN(setq, "setq");
DEFUN(set, "set");

DEFUN(print, "print");
DEFUN(println, "println");

DEFUN(quote, "quote");
DEFUN(function, "function");

DEFUN(if, "if");
DEFUN(while, "while");
DEFUN(dolist, "dolist");
DEFUN(cond, "cond");

DEFUN(mapc, "mapc");

DEFUN(or, "or");
DEFUN(and, "and");
DEFUN(not, "not");

DEFUN(unless, "unless");
DEFUN(when, "when");

DEFUN(progn, "progn");
DEFUN(letx, "let*");
DEFUN(let, "let");

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

DEFUN(psym, "psym");
DEFUN(plist, "plist");
DEFUN(pint, "pint");
DEFUN(preal, "preal");
DEFUN(pstring, "pstring");
DEFUN(pfunction, "pfunction");


DEFUN(exit, "exit");
DEFUN(dump, "dump");






}
