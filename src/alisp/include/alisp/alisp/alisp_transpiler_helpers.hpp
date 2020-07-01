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



inline void module_define_function(env::Module *t_module,
const ALObjectPtr t_sym,
ALObjectPtr t_params,
ALObjectPtr t_body,
std::string t_doc = {})
{
    auto &scope = t_module->root_scope();
    auto name   = t_sym->to_string();
            
    AL_DEBUG("Defining function: "s += name);

    NameValidator::validate_object_name(name);

    AL_CHECK(if (scope.count(name)) { throw environment_error("Function alredy exists: " + name); });

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_prop("--module--", make_string(t_module->name()));
    new_fun->set_prop("--name--", make_string(name));

#ifdef ENABLE_OBJECT_DOC
    new_fun->set_prop("--doc--", make_string(t_doc));
#endif

    scope.insert({ name, new_fun });
}

inline void
module_define_variable(env::Module *t_module, const ALObjectPtr t_sym, ALObjectPtr t_value, std::string t_doc = {})
{
    auto &scope = t_module->root_scope();
    auto name   = t_sym->to_string();

    AL_DEBUG("New variable: "s += name);

    NameValidator::validate_object_name(name);

    AL_CHECK(if (scope.count(name)) { throw environment_error("Variable alredy exists: " + name); });
    t_value->set_prop("--module--", make_string(t_module->name()));

#ifdef ENABLE_OBJECT_DOC
    t_value->set_prop("--doc--", make_string(t_doc));
#endif

    scope.insert({ name, t_value });
}

inline void module_define_macro(env::Module *t_module,
const ALObjectPtr t_sym,
ALObjectPtr t_params,
ALObjectPtr t_body,
std::string t_doc = {})
{
    auto &scope = t_module->root_scope();
    auto name   = t_sym->to_string();

    AL_DEBUG("Defining macro: "s += name);

    NameValidator::validate_object_name(name);

    AL_CHECK(if (scope.count(name)) { throw environment_error("Function alredy exists: " + name); });

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_macro_flag();
    new_fun->set_prop("--module--", make_string(t_module->name()));
    new_fun->set_prop("--name--", make_string(name));

#ifdef ENABLE_OBJECT_DOC
    new_fun->set_prop("--doc--", make_string(t_doc));
#endif

    scope.insert({ name, new_fun });
}

}
