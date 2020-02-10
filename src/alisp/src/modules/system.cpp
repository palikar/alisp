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
#include "alisp/utility/env.hpp"

#include <stdlib.h> 

namespace alisp
{

namespace detail
{


ALObjectPtr get_evnvars()
{
    ALObject::list_type list_env;

    for (auto& [name, var] : utility::env_list()) {
        list_env.push_back(make_object(name, var));
    }

    return make_object(list_env);
}

ALObjectPtr Fget_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto var = eval->eval(t_obj->i(0));
    assert_string(var);
    return make_string(utility::env_string(var->to_string().c_str()));
}

ALObjectPtr Fcheck_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto var= eval->eval(t_obj->i(0));
    assert_string(var);

    return utility::env_bool(var->to_string().c_str()) ? Qt : Qnil;
}

ALObjectPtr Fset_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto var = eval->eval(t_obj->i(0));
    auto val = eval->eval(t_obj->i(1));
    assert_string(var);
    assert_string(val);

    utility::env_set(var->to_string(), val->to_string());
    
    return Qt;
}

ALObjectPtr Flist_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator*)
{
    assert_size<0>(t_obj);
    return get_evnvars();
}

ALObjectPtr Fchwd(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;
    
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);
    const auto p = path->to_string();

    if (!fs::exists(p)) {
        return Qnil;
    }

    fs::current_path(p);

    return Qt;
}

ALObjectPtr Fsystem(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto command = eval->eval(t_obj->i(0));
    assert_string(command);

    if (system(command->to_string().c_str())) {
        return Qt;
    } else {
        return Qnil;
    };
}


}

env::ModulePtr init_system(env::Environment *, eval::Evaluator *)
{
    auto Msystem = module_init("system");
    auto sys_ptr = Msystem.get();

    module_defvar(sys_ptr, "env-vars", detail::get_evnvars());

    module_defun(sys_ptr, "get-env", &detail::Fget_env);
    module_defun(sys_ptr, "check-env", &detail::Fcheck_env);
    module_defun(sys_ptr, "set-env", &detail::Fset_env);
    module_defun(sys_ptr, "list-env", &detail::Flist_env);
    module_defun(sys_ptr, "chwd", &detail::Fchwd);
    module_defun(sys_ptr, "sys", &detail::Fsystem);

    return Msystem;
}


}  // namespace alisp
