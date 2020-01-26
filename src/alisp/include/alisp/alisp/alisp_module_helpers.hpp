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
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_factory.hpp"


namespace alisp
{

inline auto module_init(std::string t_name) {
    return std::make_shared<env::Module>(std::move(t_name));
}

inline void module_defun(env::Module* t_module, std::string t_name, Prim::func_type fun) {
    auto& new_fun = t_module->get_root().insert({std::move(t_name), make_prime(fun, t_name)}).first->second;
    new_fun->set_function_flag();
    new_fun->set_prop("--module--", make_string(t_module->name()));          
}

inline void module_defvar(env::Module* t_module, std::string t_name, ALObjectPtr val) {
    auto& new_var = t_module->get_root().insert({std::move(t_name), std::move(val)}).first->second;
    new_var->set_prop("--module--", make_string(t_module->name()));
}

inline void module_defconst(env::Module* t_module, std::string t_name, ALObjectPtr val) {
    auto& new_var = t_module->get_root().insert({std::move(t_name), std::move(val)}).first->second;
    new_var->set_const_flag();
    new_var->set_prop("--module--", make_string(t_module->name()));
}


inline void module_dump(env::Module* t_module) {
    for (auto& [name, sym]  : t_module->root_scope()) {
        std::cout << name << " : " << sym << "\n";
    }
}






}

