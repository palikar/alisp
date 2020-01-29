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
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    return Qnil;
}

ALObjectPtr Fcheck_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    return Qnil;
}

ALObjectPtr Fset_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    return Qnil;
}

ALObjectPtr Flist_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    return Qnil;
}

ALObjectPtr Fchwd(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fsystem(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}


}

env::ModulePtr init_system(env::Environment *, eval::Evaluator *)
{

    auto Msystem = module_init("system");
    auto sys_ptr = Msystem.get();

    module_defvar(sys_ptr, "env-vars", detail::get_evnvars());
    
    return Msystem;
}


}  // namespace alisp
