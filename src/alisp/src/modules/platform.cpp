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


namespace alisp
{

namespace detail
{


}  // namespace detail

env::ModulePtr init_platform(env::Environment *, eval::Evaluator *)
{

    auto Mplatform = module_init("platform");
    auto plat_ptr  = Mplatform.get();

    module_doc(plat_ptr, R"(The `platform` module exposes infomration about the Alisp
interpreter, the underlying operating system and information about it
as well as how the intrpterter was compiled.)");

    module_defvar(plat_ptr, "os", make_string(ALISP_OS_NAME));
    module_defvar(plat_ptr, "alisp-version", make_string(alisp::BuildInfo::version()));
    module_defvar(plat_ptr, "alisp-version-major", make_int(alisp::BuildInfo::version_major()));
    module_defvar(plat_ptr, "alisp-version-minor", make_int(alisp::BuildInfo::version_minor()));
    module_defvar(plat_ptr, "alisp-version-patch", make_int(alisp::BuildInfo::version_patch()));
    module_defvar(plat_ptr, "max-call-depth", make_int(MAX_FUNCTION_CALL_DEPTH));
    module_defvar(plat_ptr, "max-evaluation-depth", make_int(MAX_EAVALUATION_DEPTH));

    module_defvar(plat_ptr, "compiler-name", make_string(ALISP_COMPILER_NAME));
    module_defvar(plat_ptr, "compiler-version", make_string(compiler_version));

    module_defvar(plat_ptr, "arch", make_string(ALISP_ARCH_NAME));

    module_defvar(plat_ptr, "debug-build", debug_build ? Qt : Qnil);
    module_defvar(plat_ptr, "build-info", make_string(get_build_info()));

    return Mplatform;
}


}  // namespace alisp
