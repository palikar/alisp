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


#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_signature.hpp"

#include "alisp/alisp/declarations/constants.hpp"

namespace alisp
{

// Used by the module files to consturct a module object

inline auto module_init(std::string t_name)
{
    return std::make_shared<env::Module>(std::move(t_name));
}

inline void module_eval(env::Module *t_module, std::string t_eval)
{
    t_module->eval_string(std::move(t_eval));
}

inline void module_eval(env::Module *t_module, ALObjectPtr t_obj)
{
    t_module->eval_obj(std::move(t_obj));
}

inline void module_defun(env::Module *t_module,
                         std::string t_name,
                         Prim::func_type fun,
                         std::string t_doc     = {},
                         ALObjectPtr signature = Qnil,
                         bool managed          = true)
{
    auto &new_fun = t_module->get_root().insert({ t_name, make_prime(fun, t_name) }).first->second;
    new_fun->set_function_flag();

#ifdef ENABLE_OBJECT_DOC
    new_fun->set_prop("--doc--", make_string(t_doc));
#endif
    new_fun->set_prop("--name--", make_string(t_name));
    new_fun->set_prop("--module--", make_string(t_module->name()));

    if (signature != Qnil)
    {
        new_fun->set_prop("--signature--", signature);
    }

    if (managed)
    {
        new_fun->set_prop("--managed--", Qt);
    }
}

inline void module_defvar(env::Module *t_module, std::string t_name, ALObjectPtr val, std::string t_doc = {})
{
    auto &new_var = t_module->get_root().insert({ t_name, std::move(val) }).first->second;

#ifdef ENABLE_OBJECT_DOC
    new_var->set_prop("--doc--", make_string(t_doc));
#endif
    new_var->set_prop("--name--", make_string(t_name));
    new_var->set_prop("--module--", make_string(t_module->name()));
}

inline void module_defconst(env::Module *t_module, std::string t_name, ALObjectPtr val, std::string t_doc = {})
{
    auto &new_var = t_module->get_root().insert({ t_name, std::move(val) }).first->second;
    new_var->set_const_flag();

#ifdef ENABLE_OBJECT_DOC
    new_var->set_prop("--doc--", make_string(t_doc));
#endif
    new_var->set_prop("--module--", make_string(t_module->name()));
    new_var->set_prop("--name--", make_string(t_name));
}

inline void module_doc(env::Module *t_module, std::string t_doc)
{
    module_defconst(t_module, "--doc--", make_string(t_doc));
}


}  // namespace alisp
