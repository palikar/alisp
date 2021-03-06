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

struct get_env
{
    static inline const std::string name{ "get-env" };

    static inline const std::string doc{ R"((get-env VAR)

Return the value of the environment variable `VAR` if avaialble. Return `nil` otherwise.
)" };

    inline static const Signature signature{ String{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto var = arg_eval(eval, obj, 0);
        return make_string(utility::env_string(var->to_string().c_str()));
    }
};

struct check_env
{
    static inline const std::string name{ "check-env" };

    static inline const std::string doc{ R"((check-env VAR)

Return the `t` if the environment variable `VAR` is defined. Return `nil` otherwise.
)" };

    inline static const Signature signature{ String{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto var = arg_eval(eval, obj, 0);

        return utility::env_bool(var->to_string().c_str()) ? Qt : Qnil;
    }
};

struct set_env
{
    static inline const std::string name{ "set-env" };

    static inline const std::string doc{ R"((set-env VAR VALUE)

Set the value of the environment variable `VAR` to `VALUE`
)" };

    inline static const Signature signature{ String{}, String{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto var = arg_eval(eval, obj, 0);
        auto val = arg_eval(eval, obj, 1);

        utility::env_set(var->to_string(), val->to_string());

        return Qt;
    }
};

struct list_env
{
    static inline const std::string name{ "list-env" };

    static inline const std::string doc{ R"( Return the value of `env-vars`.)" };

    inline static const Signature signature{};

    static ALObjectPtr func(const ALObjectPtr &, env::Environment *, eval::Evaluator *) { return get_evnvars(); }
};

struct chwd
{
    static inline const std::string name{ "chwd" };

    static inline const std::string doc{ R"((chwd PATH)

Change the currnt working directory to `PATH`.
)" };

    inline static const Signature signature{ String{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        auto path    = arg_eval(eval, obj, 0);
        const auto p = path->to_string();

        if (!fs::exists(p))
        {
            return Qnil;
        }

        fs::current_path(p);

        return Qt;
    }
};

struct system
{
    static inline const std::string name{ "sys" };

    static inline const std::string doc{ R"((sys COMMAND)

Execute the command `COMMAND` in a shell of the host system.
)" };

    inline static const Signature signature{ String{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto command = arg_eval(eval, obj, 0);

        if (::system(command->to_string().c_str()))
        {
            return Qt;
        }
        else
        {
            return Qnil;
        };
    }
};


struct module_doc
{
    inline static const std::string doc{
        R"(The `os` modules allows you to access common OS functions through Alisp.)"
    };
};

}  // namespace detail

env::ModulePtr init_system(env::Environment *, eval::Evaluator *)
{
    auto Msystem = module_init("system");
    auto sys_ptr = Msystem.get();

    module_doc(sys_ptr, detail::module_doc::doc);

    using namespace detail;

    module_defun(sys_ptr, get_env::name, get_env::func, get_env::doc, get_env::signature.al());
    module_defun(sys_ptr, check_env::name, check_env::func, check_env::doc, check_env::signature.al());
    module_defun(sys_ptr, set_env::name, set_env::func, set_env::doc, set_env::signature.al());
    module_defun(sys_ptr, list_env::name, list_env::func, list_env::doc, list_env::signature.al());
    module_defun(sys_ptr, chwd::name, chwd::func, chwd::doc, chwd::signature.al());
    module_defun(sys_ptr, system::name, system::func, system::doc, system::signature.al());

    return Msystem;
}


}  // namespace alisp
