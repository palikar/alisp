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

    for (auto &[name, var] : utility::env_list())
    {
        list_env.push_back(make_object(name, var));
    }

    return make_object(list_env);
}

ALObjectPtr Fget_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto var = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(var));
    return make_string(utility::env_string(var->to_string().c_str()));
}

ALObjectPtr Fcheck_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto var = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(var));

    return utility::env_bool(var->to_string().c_str()) ? Qt : Qnil;
}

ALObjectPtr Fset_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(t_obj));
    auto var = eval->eval(t_obj->i(0));
    auto val = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(var));
    AL_CHECK(assert_string(val));

    utility::env_set(var->to_string(), val->to_string());

    return Qt;
}

ALObjectPtr Flist_env(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));
    return get_evnvars();
}

ALObjectPtr Fchwd(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    const auto p = path->to_string();

    if (!fs::exists(p))
    {
        return Qnil;
    }

    fs::current_path(p);

    return Qt;
}

ALObjectPtr Fsystem(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto command = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(command));

    if (system(command->to_string().c_str()))
    {
        return Qt;
    }
    else
    {
        return Qnil;
    };
}


}  // namespace detail

env::ModulePtr init_system(env::Environment *, eval::Evaluator *)
{
    auto Msystem = module_init("system");
    auto sys_ptr = Msystem.get();

    module_doc(sys_ptr,
               "The `os` modules allows you to access common OS functions "
               "through Alisp.");


    module_defvar(sys_ptr,
                  "env-vars",
                  detail::get_evnvars(),
                  R"(A list of pairs of the form `(VAR VALUE)` where `VAR` is an
envoronment variable and `VALUE` is its value. The list contain all of
the currently bounded environment variables.
)");


    module_defun(sys_ptr,
                 "get-env",
                 &detail::Fget_env,
                 R"((get-env VAR)

Return the value of the environment variable `VAR` if avaialble. Return `nil` otherwise.
)");

    module_defun(sys_ptr,
                 "check-env",
                 &detail::Fcheck_env,
                 R"((check-env VAR)

Return the `t` if the environment variable `VAR` is defined. Return `nil` otherwise.
)");

    module_defun(sys_ptr,
                 "set-env",
                 &detail::Fset_env,
                 R"((set-env VAR VALUE)

Set the value of the environment variable `VAR` to `VALUE`
)");

    module_defun(sys_ptr,
                 "list-env",
                 &detail::Flist_env,
                 R"( Return the value of `env-vars`.)");

    module_defun(sys_ptr,
                 "chwd",
                 &detail::Fchwd,
                 R"((chwd PATH)

Change the currnt working directory to `PATH`.
)");

    module_defun(sys_ptr,
                 "sys",
                 &detail::Fsystem,
                 R"((sys COMMAND)

Execute the command `COMMAND` in a shell of the host system.
)");


    return Msystem;
}


}  // namespace alisp
