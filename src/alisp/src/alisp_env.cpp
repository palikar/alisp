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


#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/declarations/constants.hpp"

#include "alisp/utility/macros.hpp"

#include <fmt/format.h>


namespace alisp
{

namespace env
{

Environment::Environment()
  : m_modules{ { "--main--", std::make_shared<Module>("--main--") } }
  , m_active_module({ *m_modules.at("--main--").get() })
  , m_call_depth(0)
{
}

Environment::~Environment()
{
    g_user_symbols.clear();
}

ALObjectPtr Environment::find(const ALObjectPtr &t_sym)
{

    const auto name = t_sym->to_string();

    AL_DEBUG("Finding a symbol: "s += name);

    for (auto &scope : utility::reverse(m_stack.current_frame()))
    {
        if (scope.count(name))
        {
            return scope.at(name);
        };
    }

    if (g_prime_values.count(name))
    {
        return g_prime_values.at(name);
    }

    if (m_active_module.get().root_scope().count(name))
    {
        return m_active_module.get().root_scope().at(name);
    };

    if (m_modules.at("--main--")->root_scope().count(name))
    {
        return m_modules.at("--main--")->root_scope().at(name);
    };

    throw environment_error("Unbounded Symbol: " + name);
}

void Environment::update(const ALObjectPtr &t_sym, ALObjectPtr t_value)
{
    const auto name = t_sym->to_string();

    for (auto &scope : m_stack.current_frame())
    {
        if (scope.count(name))
        {
            auto &sym = scope.at(name);
            AL_CHECK(
              if (sym->check_const_flag()) { throw environment_error("Trying to change a const symbol: " + name); });
            sym = std::move(t_value);
            return;
        };
    }

    if (m_active_module.get().root_scope().count(name))
    {
        auto &sym = m_active_module.get().root_scope().at(name);
        AL_CHECK(if (sym->check_const_flag()) { throw environment_error("Trying to change a const symbol: " + name); });
        sym = std::move(t_value);
        return;
    };

    throw environment_error("Unbounded Symbol: " + name);
}

void Environment::put(const ALObjectPtr &t_sym, ALObjectPtr t_val)
{
    auto &scope = m_stack.current_scope();
    auto name   = t_sym->to_string();

    // NameValidator::validate_object_name(name);

    if (scope.count(name))
    {
        scope.at(name) = t_val;
        warn::warn_env("Putting an already existent variable in the env: "s += name);
        // throw environment_error("Variable alredy exists: " + name);
    }

    scope.insert({ name, std::move(t_val) });
}

void Environment::define_variable(const ALObjectPtr &t_sym, ALObjectPtr t_value, std::string t_doc, bool t_const)
{
    auto &scope = m_active_module.get().root_scope();
    auto name   = t_sym->to_string();

    AL_DEBUG("New variable: "s += name);

    NameValidator::validate_object_name(name);

    AL_CHECK(if (scope.count(name)) { throw environment_error("Variable alredy exists: " + name); });
    t_value->set_prop("--name--", make_string(name));
    t_value->set_prop("--module--", make_string(m_active_module.get().name()));

#ifdef ENABLE_OBJECT_DOC
    t_value->set_prop("--doc--", make_string(t_doc));
#endif

    if (t_const)
    {
        t_value->set_const_flag();
    }

    scope.insert({ name, std::move(t_value) });
}

void Environment::define_function(const ALObjectPtr &t_sym, ALObjectPtr t_params, ALObjectPtr t_body, std::string t_doc)
{

    auto &scope = m_active_module.get().root_scope();
    auto name   = t_sym->to_string();

    AL_DEBUG("Defining function: "s += name);

    NameValidator::validate_object_name(name);

    AL_CHECK(if (scope.count(name)) { throw environment_error("Function alredy exists: " + name); });

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_prop("--module--", make_string(m_active_module.get().name()));
    new_fun->set_prop("--name--", make_string(name));

#ifdef ENABLE_OBJECT_DOC
    new_fun->set_prop("--doc--", make_string(t_doc));
#endif

    scope.insert({ name, std::move(new_fun) });
}

void Environment::define_macro(const ALObjectPtr &t_sym, ALObjectPtr t_params, ALObjectPtr t_body, std::string t_doc)
{

    auto &scope = m_active_module.get().root_scope();
    auto name   = t_sym->to_string();

    AL_DEBUG("Defining macro: "s += name);

    NameValidator::validate_object_name(name);

    AL_CHECK(if (scope.count(name)) { throw environment_error("Function alredy exists: " + name); });

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_macro_flag();
    new_fun->set_prop("--module--", make_string(m_active_module.get().name()));
    new_fun->set_prop("--name--", make_string(name));

#ifdef ENABLE_OBJECT_DOC
    new_fun->set_prop("--doc--", make_string(t_doc));
#endif

    scope.insert({ name, std::move(new_fun) });
}

void Environment::activate_module(const std::string &t_name)
{
    Vcurrent_module->set(t_name);
    m_active_module = *m_modules.at(t_name).get();
}

bool Environment::load_builtin_module(const std::string &t_module_name, eval::Evaluator *eval)
{
    auto module_import = g_builtin_modules.find(t_module_name);
    if (module_import == std::end(g_builtin_modules))
    {
        return false;
    }

    AL_DEBUG("Loading builitng dyn module: "s += t_module_name);

    auto new_mod = module_import->second.function(this, eval);
    m_modules.insert({ t_module_name, new_mod });

    detail::ModuleChange mc{ *this, t_module_name };
    for (auto &eval_str : new_mod->eval_strings())
    {
        eval->eval_string(eval_str);
    }
    for (auto &eval_obj : new_mod->eval_objs())
    {
        eval->eval(eval_obj);
    }

    return true;
}

void Environment::load_module(eval::Evaluator *eval, const std::string t_file, const std::string &t_name)
{
    AL_DEBUG("Loading dyn modules: "s += t_name + " from " + t_file);
    auto loaded_mod = m_loaded_modules.insert({ t_name, std::make_unique<dynmoduels::AlispDynModule>(t_name, t_file) })
                        .first->second.get();
    auto mod_ptr = loaded_mod->init_dynmod(this, eval);
    define_module(t_name, mod_ptr);

    alias_module(t_name, t_name);

    detail::ModuleChange mc{ *this, t_name };
    for (auto &eval_str : mod_ptr->eval_strings())
    {
        eval->eval_string(eval_str);
    }
    for (auto &eval_obj : mod_ptr->eval_objs())
    {
        eval->eval(eval_obj);
    }
}

void Environment::import_root_scope(const std::string &t_from, const std::string &t_to)
{

    AL_DEBUG("Importing module symbols: "s += t_from + " -> " + t_to);
    auto &from_root = m_modules.at(t_from)->get_root();
    auto &to_root   = m_modules.at(t_to)->get_root();

    if (from_root.empty())
    {
        warn::warn_import("Importing an empty root scope.");
    }

    for (auto &[name, sym] : from_root)
    {
        AL_DEBUG("Adding a symbol: "s += name);
        if (!sym->prop_exists("--module--"))
        {
            continue;
        }
        if (sym->get_prop("--module--")->to_string().compare(t_from) == 0)
        {
            to_root.insert({ name, sym });
        }
    }

    // m_modules.at(t_to)->get_root() = m_modules.at(t_from)->get_root();
}

Module *Environment::get_module(const std::string &t_name)
{
    auto mod = m_modules.find(t_name);
    if (mod != std::end(m_modules))
    {
        return mod->second.get();
    };
    warn::warn_env("Referencing non existen module:"s += t_name);
    return nullptr;
}

void Environment::alias_module(const std::string &t_name, const std::string &t_alias)
{
    if (m_modules.find(t_name) == m_modules.end())
    {
        AL_CHECK(warn::warn_env("Referencing non existen module:"s += t_name););
        return;
    }
    AL_DEBUG("Aliasing a module: "s += t_name + " -> " + t_alias);
    m_active_module.get().add_module(m_modules.at(t_name), t_alias);
}

void Environment::defer_callback(std::function<void()> t_callback)
{

    if (m_stack.current_frame().size() == 1)
    {
        return;
    }
    m_deferred_calls.emplace_back(m_stack.stacks.size(), m_stack.current_frame().size(), t_callback);
}

void Environment::define_module(const std::string t_name, const std::string)
{
    AL_DEBUG("Creating a new module: "s += t_name);
    auto new_mod = std::make_shared<Module>(t_name);
    m_modules.insert({ std::move(t_name), new_mod });
}

void Environment::define_module(const std::string t_name, ModulePtr t_mod)
{
    AL_DEBUG("Adding a new module: "s += t_name);
    m_modules.insert({ std::move(t_name), std::move(t_mod) });
}

bool Environment::module_loaded(const std::string &t_module_name)
{
    return m_modules.count(t_module_name) != 0;
}

void Environment::resolve_callbacks()
{
    while (!m_deferred_calls.empty()
           and (m_stack.current_frame().size() <= std::get<0>(m_deferred_calls.back())
                and m_stack.stacks.size() <= std::get<1>(m_deferred_calls.back())))
    {
        std::invoke(std::get<2>(m_deferred_calls.back()));
        m_deferred_calls.pop_back();
    }
}

void Environment::stack_dump() const
{
    using namespace fmt;
    using namespace std;

    cout.flush();

    cout << format("+{:-^48}+", "Stack") << '\n';

    size_t frame_index    = 0;
    const auto stack_size = std::size(m_stack.stacks);

    for (auto &frame : m_stack.stacks)
    {
        cout << format("|{:^48}|", format("Frame {}", frame_index)) << '\n';
        cout << format("|{:^48}|", "") << '\n';

        cout << format("+{:-^10}+", "");
        cout << format("{:-^37}+", "") << '\n';

        size_t scope_index = 0;

        for (auto &scope : frame)
        {

            if (scope_index != 0)
            {
                cout << format("+{:-^10}+", "");
                cout << format("{:-^37}+", "") << '\n';
            }

            cout << format("|{:<10}|", format("Scope {}", scope_index));
            cout << format("{:<37}|", "") << '\n';

            cout << format("+{:-^10}+", "");
            cout << format("{:-^37}+", "") << '\n';

            for (auto &[sym, val] : scope)
            {
                cout << format("|{:<10}|", "");

                std::cout << format("{:<37}|", sym) << '\n';
            }

            ++scope_index;
        }

        if (frame_index != stack_size - 1)
        {
            cout << format("+{:-^10}+", "");
            cout << format("{:-^37}+", "") << '\n';
        }

        ++frame_index;
    }


    cout << format("+{:-^10}+", "");
    cout << format("{:-^37}+", "") << '\n';
}

void Environment::env_dump() const
{

    using namespace fmt;
    using namespace std;

    cout.flush();

    cout << format("+{:-^48}+", "Environment") << '\n';

    cout << format("+{:-^15}+{:-^32}+", "", "") << '\n';
    cout << format("|{:<15}|{:^32}|", "Primitives", "") << '\n';
    cout << format("+{:-^15}+{:-^32}+", "", "") << '\n';

    size_t index = 1;
    for (auto &[sym, _] : g_prime_values)
    {
        cout << format("|{:<5}{:^43}|", index++, sym) << '\n';
    }


    for (auto &[mod_name, mod] : m_modules)
    {
        cout << format("+{:-^15}+{:-^32}+", "", "") << '\n';
        cout << format("|{:<15}|{:^32}|", mod_name, "") << '\n';
        cout << format("+{:-^15}+{:-^32}+", "", "") << '\n';

        index = 1;
        for (auto &[sym, _] : mod->get_root())
        {
            cout << format("|{:<5}{:^43}|", index++, sym) << '\n';
        }
    }

    cout << format("+{:-^48}+", "") << '\n';
}

void Environment::callstack_dump() const
{

#ifdef ENABLE_STACK_TRACE
    using namespace fmt;
    using namespace std;

    cout.flush();
    cout << format("+{:-^48}+", "Call stack") << '\n';

    for (auto &[fun, prime] : utility::reverse(m_stack_trace))
    {

        if (prime)
        {
            cout << format("|{:<46}{:>}|", fun, "<-") << '\n';
        }
        else
        {
            cout << format("|{:<48}|", fun) << '\n';
        }
    }
    cout << format("+{:-^48}+", "") << '\n';
#endif
}

namespace detail
{


FunctionCall::FunctionCall(Environment &t_env, ALObjectPtr t_func) : m_env(t_env)
{
    m_env.call_function();

    // unload the closure here
    if (t_func->prop_exists("--closure--"))
    {
        for (auto &el : *(t_func->get_prop("--closure--")))
        {
            t_env.put(el->i(0), el->i(1));
        }
    }

    if (t_func->prop_exists("--module--"))
    {
        auto func_module = t_func->get_prop("--module--")->to_string();
        if (m_env.current_module().compare(func_module) != 0)
        {
            m_prev_mod = m_env.current_module();
            t_env.activate_module(func_module);
        }

        return;
    }

    warn::warn_env("Calling a function without --module-- property.");
}

FunctionCall::~FunctionCall()
{
    if (!m_prev_mod.empty())
    {
        m_env.activate_module(m_prev_mod);
    }
    m_env.finish_function();
}

ScopePushPop::ScopePushPop(Environment &t_env) : m_env(t_env)
{
    m_env.new_scope();
}

ScopePushPop::~ScopePushPop()
{
    m_env.destroy_scope();
    m_env.resolve_callbacks();
}

MacroCall::MacroCall(Environment &t_env) : m_env(t_env)
{
    m_env.new_scope();
}

MacroCall::~MacroCall()
{
    m_env.destroy_scope();
}

ModuleChange::ModuleChange(Environment &t_env, const std::string &t_module)
  : m_env(t_env), m_prev_mod(m_env.current_module())
{

    m_env.activate_module(t_module);
}

ModuleChange::~ModuleChange()
{
    m_env.activate_module(m_prev_mod);
}

}  // namespace detail

}  // namespace env

}  // namespace alisp
