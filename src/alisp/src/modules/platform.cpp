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

struct os_var
{
    static inline const std::string name{ "os" };

    static inline const std::string doc{ R"(The name of the current os.
The value can be:
  * linux
  * windows-32
  * windows-64
  * max-osx
  * max-osx
  * freebsd
  * unix
  * unknown
)" };

    static inline const auto var = make_string(ALISP_OS_NAME);
};

struct alisp_version_var
{
    static inline const std::string name{ "alisp-version" };

    static inline const std::string doc{ R"(A string of the version of the alisp interpreter.
)" };

    static inline const auto var = make_string(alisp::BuildInfo::version());
};

struct alisp_version_major_var
{
    static inline const std::string name{ "alisp-version-major" };

    static inline const std::string doc{ R"(The major versin of the alisp interpreter.
)" };

    static inline const auto var = make_int(alisp::BuildInfo::version_major());
};

struct alisp_version_minor_var
{
    static inline const std::string name{ "alisp-version-minor" };

    static inline const std::string doc{ R"(The minor versin of the alisp interpreter.
)" };

    static inline const auto var = make_int(alisp::BuildInfo::version_minor());
};

struct alisp_version_patch_var
{
    static inline const std::string name{ "alisp-version-patch" };

    static inline const std::string doc{ R"(The patch versin of the alisp interpreter.
)" };

    static inline const auto var = make_int(alisp::BuildInfo::version_patch());
};

struct max_call_depth_var
{
    static inline const std::string name{ "max-call-depth" };

    static inline const std::string doc{ R"(The maximum number of function calls that can be nested in one another.
)" };

    static inline const auto var = make_int(MAX_FUNCTION_CALL_DEPTH);
};

struct max_eval_depth_var
{
    static inline const std::string name{ "max-evaluation-depth" };

    static inline const std::string doc{ R"(The maximum number of evalution nesting that the evaluator
supports. This is the depth limit of drcptrddoind in alisp.
)" };

    static inline const auto var = make_int(MAX_EAVALUATION_DEPTH);
};

struct compiler_name_var
{
    static inline const std::string name{ "compiler-name" };

    static inline const std::string doc{ R"(The name of the compiler with which the interpreter was compiled.
)" };

    static inline const auto var = make_string(ALISP_COMPILER_NAME);
};

struct compiler_version_var
{
    static inline const std::string name{ "compiler-version" };

    static inline const std::string doc{
        R"(The vesion of the compiler (in a string format) with which the interpreter was compiled.
)"
    };

    static inline const auto var = make_string(compiler_version);
};

struct arch_var
{
    static inline const std::string name{ "arch" };

    static inline const std::string doc{ R"(The computer architecture that the interpreter is running on. 
Possible
values are:
  * i386
  * x86_64
  * arm
  * power64pc
  * aarch64
  * unknown

 )" };

    static inline const auto var = make_string(ALISP_ARCH_NAME);
};

struct module_doc
{
    inline static const std::string doc{ R"(The `platform` module exposes infomration about the Alisp
interpreter, the underlying operating system and information about it
as well as how the intrpterter was compiled.
)" };
};

}  // namespace detail

env::ModulePtr init_platform(env::Environment *, eval::Evaluator *)
{

    auto Mplatform = module_init("platform");
    auto plat_ptr  = Mplatform.get();

    module_doc(plat_ptr, detail::module_doc::doc);

    using namespace detail;


    module_defvar(plat_ptr, os_var::name, os_var::var);
    module_defvar(plat_ptr, arch_var::name, arch_var::var);
    module_defvar(plat_ptr, alisp_version_var::name, alisp_version_var::var);
    module_defvar(plat_ptr, alisp_version_major_var::name, alisp_version_major_var::var);
    module_defvar(plat_ptr, alisp_version_minor_var::name, alisp_version_minor_var::var);
    module_defvar(plat_ptr, alisp_version_patch_var::name, alisp_version_patch_var::var);
    module_defvar(plat_ptr, max_call_depth_var::name, max_call_depth_var::var);
    module_defvar(plat_ptr, max_eval_depth_var::name, max_eval_depth_var::var);
    module_defvar(plat_ptr, compiler_name_var::name, compiler_name_var::var);
    module_defvar(plat_ptr, compiler_version_var::name, compiler_version_var::var);


    return Mplatform;
}


}  // namespace alisp
