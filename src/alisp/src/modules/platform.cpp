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

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/config.hpp"


namespace alisp
{

namespace detail
{


}  // namespace detail

env::ModulePtr init_platform(env::Environment *, eval::Evaluator *)
{

    auto Mplatform = module_init("platform");
    auto plat_ptr  = Mplatform.get();

    module_doc(plat_ptr,
               R"(The `platform` module exposes infomration about the Alisp
interpreter, the underlying operating system and information about it
as well as how the intrpterter was compiled.)");


    module_defvar(plat_ptr,
                  "os",
                  make_string(ALISP_OS_NAME),
                  R"(

The name of the current os. The value can be:
* linux
* windows-32
* windows-64
* max-osx
* max-osx
* freebsd
* unix
* unknown

)");

    module_defvar(plat_ptr,
                  "alisp-version",
                  make_string(alisp::BuildInfo::version()),
                  R"(

A string of the version of the alisp interpreter.
)");

    module_defvar(plat_ptr,
                  "alisp-version-major",
                  make_int(alisp::BuildInfo::version_major()),
                  R"(

The major versin of the alisp interpreter.
)");

    module_defvar(plat_ptr,
                  "alisp-version-minor",
                  make_int(alisp::BuildInfo::version_minor()),
                  R"(

The minor versin of the alisp interpreter.
)");

    module_defvar(plat_ptr,
                  "alisp-version-patch",
                  make_int(alisp::BuildInfo::version_patch()),
                  R"(

The patch versin of the alisp interpreter.
)");

    module_defvar(plat_ptr,
                  "max-call-depth",
                  make_int(MAX_FUNCTION_CALL_DEPTH),
                  R"(

The maximum number of function calls that can be nested in one another.
)");

    module_defvar(plat_ptr,
                  "max-evaluation-depth",
                  make_int(MAX_EAVALUATION_DEPTH),
                  R"(

The maximum number of evalution nesting that the evaluator
supports. This is the depth limit of drcptrddoind in alisp.
)");


    module_defvar(plat_ptr,
                  "compiler-name",
                  make_string(ALISP_COMPILER_NAME),
                  R"(

The name of the compiler with which the interpreter was compiled.
)");

    module_defvar(plat_ptr,
                  "compiler-version",
                  make_string(compiler_version),
                  R"(

The vesion of the compiler (in a string format) with which the interpreter was compiled.
)");


    module_defvar(plat_ptr,
                  "arch",
                  make_string(ALISP_ARCH_NAME),
                  R"(

The computer architecture that the interpreter is running on. Possible
values are:
* i386
* x86_64
* arm
* power64pc
* aarch64
* unknown

 )");


    module_defvar(plat_ptr,
                  "debug-build",
                  debug_build ? Qt : Qnil,
                  R"(

`t` if the the interpreter was compiled in debug mode.
)");

    module_defvar(plat_ptr,
                  "build-info",
                  make_string(get_build_info()),
                  R"(

Info string about the build of the interpreter. This gets printed out
when the interpreter is started.
)");


    module_defvar(plat_ptr,
                  "disabled-checks",
                  al_disable_checks ? Qt : Qnil,
                  R"(

`t` if the interpreter was compiled without checks of argument types
by functions. This includes arity checks as well some other sanity
checks that keep the interpreter stable. Without those, you can expect
segmentaion faults when the interpreted code is invalid. However,
disabling checkes may or may not increase performance.
)");

    module_defvar(plat_ptr,
                  "enabled-documentation",
                  al_doc ? Qt : Qnil,
                  R"(

`t` if the interpreter was compiled with documentation for the
symbols. If the documentation is enabled, the majority of the symbols
will have `--doc--` property that is a string and contains the
documentaion for the symbol.
)");

    module_defvar(plat_ptr,
                  "enabled-line-trace",
                  al_line_trace ? Qt : Qnil,
                  R"(

`t` if the interpreter was compiled with support for keeping track of
the position in a file where the s-expressions were defined.
)");

    module_defvar(plat_ptr,
                  "enabled-stack-trace",
                  al_stack_trace ? Qt : Qnil,
                  R"(

`t` if the interpreter was compiled with support for keeping of the
called functions. If stack tracing is enabled, on error the
interpreter will print out the state of the stack at the moment of the
error.
)");


    return Mplatform;
}


}  // namespace alisp
