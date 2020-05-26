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


#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"


namespace alisp
{

#ifdef LINK_MODULES

extern env::ModulePtr init_fileio(env::Environment *, eval::Evaluator *);
extern env::ModulePtr init_system(env::Environment *, eval::Evaluator *);
extern env::ModulePtr init_math(env::Environment *, eval::Evaluator *);
extern env::ModulePtr init_time(env::Environment *, eval::Evaluator *);
extern env::ModulePtr init_platform(env::Environment *, eval::Evaluator *);
extern env::ModulePtr init_memory(env::Environment *, eval::Evaluator *);
extern env::ModulePtr init_async(env::Environment *, eval::Evaluator *);

#endif

namespace env
{

void init_modules()
{
#ifdef LINK_MODULES
    AL_DEBUG("Adding builltin modules."s);

    Environment::g_builtin_modules.insert({ "system", ModuleImport{ &init_system } });
    Environment::g_builtin_modules.insert({ "fileio", ModuleImport{ &init_fileio } });
    Environment::g_builtin_modules.insert({ "math", ModuleImport{ &init_math } });
    Environment::g_builtin_modules.insert({ "time", ModuleImport{ &init_time } });
    Environment::g_builtin_modules.insert({ "platform", ModuleImport{ &init_platform } });
    Environment::g_builtin_modules.insert({ "memory", ModuleImport{ &init_memory } });
    Environment::g_builtin_modules.insert({ "async", ModuleImport{ &init_async } });

#endif
}

}  // namespace env

}  // namespace alisp
