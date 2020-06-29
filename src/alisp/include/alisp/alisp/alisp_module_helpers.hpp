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

inline void module_defun(env::Module *t_module, std::string t_name, Prim::func_type fun, std::string t_doc = {})
{
    auto &new_fun = t_module->get_root().insert({ t_name, make_prime(fun, t_name) }).first->second;
    new_fun->set_function_flag();

#ifdef ENABLE_OBJECT_DOC
    new_fun->set_prop("--doc--", make_string(t_doc));
#endif
    new_fun->set_prop("--name--", make_string(t_name));
    new_fun->set_prop("--module--", make_string(t_module->name()));
}

template<typename... Args>
inline void module_signature(env::Module *t_module, std::string t_name, const Signature<Args...> &signature)
{

    if (t_module->root_scope().count(t_name) == 0)
    {
        throw std::runtime_error(
          fmt::format("{} does not exist. Report bug for the module {}", t_name, t_module->name()));
    }

    t_module->root_scope().at(t_name)->set_prop("--managed--", Qt);
    t_module->root_scope().at(t_name)->set_prop("--signature--", signature.arglist_object());
}

template<typename S> inline void module_defun(env::Module *t_module)
{
    module_defun(t_module, S::name, S::func, S::doc);
    module_signature(t_module, S::name, S::signature);
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

template<typename S> inline void module_defvar(env::Module *t_module)
{
    module_defvar(t_module, S::name, S::var);
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

inline void module_sym_doc(env::Module *t_module, std::string t_sym_name, std::string t_doc = {})
{
#ifdef ENABLE_OBJECT_DOC
    t_module->get_root().at(t_sym_name)->set_prop("--doc--", make_string(std::move(t_doc)));
#endif
}


// Used by CPP-Transpiler


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


// Used for debugging

inline void module_dump(env::Module *t_module)
{
    for (auto &[name, sym] : t_module->root_scope())
    {
        std::cout << name << " : " << sym << "\n";
    }
}


}  // namespace alisp
