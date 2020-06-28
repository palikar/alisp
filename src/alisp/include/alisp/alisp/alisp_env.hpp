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

#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>
#include <string_view>
#include <string>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_loadable_modules.hpp"
#include "alisp/alisp/alisp_warnings.hpp"

#include "alisp/utility/helpers.hpp"
#include "alisp/utility/macros.hpp"

namespace alisp::eval
{
class Evaluator;
}

namespace alisp
{


namespace env
{

extern ALObjectPtr intern(std::string name);

extern void update_prime(const ALObjectPtr &t_sym, ALObjectPtr t_val);

namespace detail
{

struct CellStack
{
  public:
    using Scope      = std::unordered_map<std::string, ALObjectPtr>;
    using StackFrame = std::vector<Scope>;
    using Stack      = std::vector<StackFrame>;

    CellStack() { push_frame(); }

    void push_frame() { stacks.emplace_back(1); }
    void pop_frame() { stacks.pop_back(); }

    void push_scope() { stacks.back().emplace_back(); }
    void pop_scope() { stacks.back().pop_back(); }

    StackFrame &root_frame() { return stacks.front(); }
    StackFrame &current_frame() { return stacks.back(); }
    Scope &current_scope() { return stacks.back().back(); }

    Stack stacks;
};

}  // namespace detail

using AlispDynModulePtr = std::unique_ptr<dynmoduels::AlispDynModule>;

class Module;

class Environment;

using ModulePtr = std::shared_ptr<Module>;

class Module
{
  public:
  private:
    detail::CellStack::Scope m_root_scope;
    std::unordered_map<std::string, ModulePtr> m_modules;
    std::string m_name;
    std::vector<std::string> m_evals;
    std::vector<ALObjectPtr> m_eval_obj;

  public:
    Module(std::string t_name) : m_name(std::move(t_name)) {}

    detail::CellStack::Scope &root_scope() { return m_root_scope; }

    std::vector<std::string> &eval_strings() { return m_evals; }
    std::vector<ALObjectPtr> &eval_objs() { return m_eval_obj; }

    void eval_string(std::string t_eval) { m_evals.push_back(std::move(t_eval)); }
    void eval_obj(ALObjectPtr t_obj) { m_eval_obj.push_back(std::move(t_obj)); }


    const std::string &name() { return m_name; }

    void add_module(ModulePtr t_module, const std::string &t_alias)
    {
        m_modules.insert({ std::move(t_alias), std::move(t_module) });
    }

    detail::CellStack::Scope &get_root() { return m_root_scope; }

    inline bool has_symbol(const std::string &t_name) { return m_root_scope.count(t_name) != 0; }
    inline bool has_module(const std::string &t_name) { return m_modules.count(t_name) != 0; }

    inline ALObjectPtr get_symbol(const std::string &t_name)
    {
        auto sym = m_root_scope.find(t_name);
        if (sym != std::end(m_root_scope))
        {
            return sym->second;
        };
        warn::warn_env(("Referencing non existent symbol "s += t_name) += " in module"s += m_name);
        return nullptr;
    }

    inline Module *get_module(const std::string &t_name)
    {
        auto mod = m_modules.find(t_name);
        if (mod != std::end(m_modules))
        {
            return mod->second.get();
        };
        warn::warn_env("Referencing non existen module:"s += t_name);
        return nullptr;
    }
};

struct ModuleImport
{
    using module_import = ModulePtr (*)(env::Environment *env, eval::Evaluator *eval);
    ModulePtr (*function)(env::Environment *env, eval::Evaluator *eval);
};

class Environment
{

  public:
    static inline std::unordered_map<std::string, ALObjectPtr> g_user_symbols;
    static inline std::unordered_map<std::string, ALObjectPtr> g_internal_symbols;
    static inline std::unordered_map<std::string, ALObjectPtr> g_prime_values;
    static inline std::unordered_map<std::string, ModuleImport> g_builtin_modules;

#ifdef ENABLE_STACK_TRACE

    struct CallElement
    {
        std::string m_function{};
        std::string m_args{};
        std::string m_file{};
        bool m_is_prime      = false;
        size_t m_line        = 0;
        size_t m_catch_depth = 0;
    };

#endif

  private:
    detail::CellStack m_stack;
    std::unordered_map<std::string, ModulePtr> m_modules;
    std::unordered_map<std::string, AlispDynModulePtr> m_loaded_modules;
    std::reference_wrapper<Module> m_active_module;

    size_t m_call_depth;

    std::vector<std::tuple<size_t, size_t, std::function<void()>>> m_deferred_calls;

#ifdef ENABLE_STACK_TRACE
    std::vector<CallElement> m_stack_trace;  // name, is_prime
    size_t m_unwind_defers{ 0 };
#endif

  public:
    Environment();

    ~Environment();

    void define_module(const std::string t_name, const std::string);

    void define_module(const std::string t_name, ModulePtr t_mod);

    void load_module(eval::Evaluator *eval, const std::string t_file, const std::string &t_name);

    void alias_module(const std::string &t_name, const std::string &t_alias);

    void activate_module(const std::string &t_name);

    inline const std::string &current_module() { return m_active_module.get().name(); }

    inline Module &current_module_ref() { return m_active_module; }

    Module *get_module(const std::string &t_name);

    bool module_loaded(const std::string &t_module_name);

    void import_root_scope(const std::string &t_from, const std::string &t_to);

    bool load_builtin_module(const std::string &t_module_name, eval::Evaluator *eval);

    ALObjectPtr find(const ALObjectPtr &t_sym);

    void define_variable(const ALObjectPtr &t_sym, ALObjectPtr t_value, std::string t_doc = {}, bool t_const = false);

    void define_function(const ALObjectPtr &t_sym, ALObjectPtr t_params, ALObjectPtr t_body, std::string t_doc = {});

    void define_macro(const ALObjectPtr &t_sym, ALObjectPtr t_params, ALObjectPtr t_body, std::string t_doc = {});

    void put(const ALObjectPtr &t_sym, ALObjectPtr t_val);

    void update(const ALObjectPtr &t_sym, ALObjectPtr t_value);

    void defer_callback(std::function<void()> t_callback);

    void resolve_callbacks();

    inline void new_scope() { m_stack.push_scope(); }

    inline void destroy_scope() { m_stack.pop_scope(); }

    inline void call_function()
    {
        ++m_call_depth;
        if (m_call_depth > MAX_FUNCTION_CALL_DEPTH)
        {
            throw environment_error("Maximum function calldepth reached!");
        }
        m_stack.push_frame();
    }

    inline void finish_function()
    {
        --m_call_depth;
        m_stack.pop_frame();
    }

    inline size_t call_depth() { return m_call_depth; }

    inline bool in_function() { return m_call_depth != 0; }

    inline bool in_root() { return (!in_function()) && (std::size(m_stack.root_frame()) == 1); }

    void stack_dump() const;

    void callstack_dump() const;

    void env_dump() const;

    inline auto get_modules() const { return m_modules; }

#ifdef ENABLE_STACK_TRACE

    void trace_call(CallElement t_traced) { m_stack_trace.push_back(std::move(t_traced)); }

    void trace_unwind()
    {
        if (m_unwind_defers > 0)
        {

            for (size_t i = 0; i < m_unwind_defers; ++i)
            {
                if (!std::empty(m_stack_trace))
                {
                    m_stack_trace.pop_back();
                }
            }
            m_unwind_defers = 0;
        }

        if (!std::empty(m_stack_trace))
        {
            m_stack_trace.pop_back();
            return;
        }

        warn::warn_env("Unwinding an empty stack.");
    }

    void defer_unwind() { ++m_unwind_defers; }

    auto &stack() { return m_stack; }

    auto get_stack_trace() -> auto & { return m_stack_trace; }

#endif
};


namespace detail
{

#ifdef ENABLE_STACK_TRACE

struct CallTracer
{
  public:
    explicit CallTracer(Environment &t_env) : m_env(t_env), m_traced() {}

    ~CallTracer()
    {
        if (m_traced.m_catch_depth == 0)
        {
            m_env.trace_unwind();
        }
        else
        {
            m_env.defer_unwind();
        }
    }

    void line(ALObject::int_type t_line) { m_traced.m_line = static_cast<size_t>(t_line); }

    void file(ALObject::string_type t_name) { m_traced.m_file = std::move(t_name); }

    void catch_depth(size_t t_catch_depth) { m_traced.m_catch_depth = t_catch_depth; }

    void function_name(std::string t_func, bool t_is_prime = false)
    {
        m_traced.m_function = std::move(t_func);
        m_traced.m_is_prime = t_is_prime;

        m_env.trace_call(m_traced);
    }

    void dump()
    {

        if (ALISP_UNLIKELY(!std::empty(m_env.get_stack_trace())))
        {
            m_env.callstack_dump();
            m_env.get_stack_trace().clear();
        }
        else
        {
            return;
        }
    }

    ALISP_RAII_OBJECT(CallTracer);

  private:
    Environment &m_env;
    Environment::CallElement m_traced;
};

#endif

struct FunctionCall
{
  public:
    explicit FunctionCall(Environment &t_env, ALObjectPtr t_func);
    ~FunctionCall();

    ALISP_RAII_OBJECT(FunctionCall);

  private:
    Environment &m_env;
    std::string m_prev_mod;
};

struct ScopePushPop
{
  public:
    explicit ScopePushPop(Environment &t_env);

    ~ScopePushPop();

    ALISP_RAII_OBJECT(ScopePushPop);

  private:
    Environment &m_env;
};

struct MacroCall
{

  public:
    explicit MacroCall(Environment &t_env);

    ~MacroCall();

    ALISP_RAII_OBJECT(MacroCall);

  private:
    Environment &m_env;
};

struct ModuleChange
{

  public:
    ModuleChange(Environment &t_env, const std::string &t_module);

    ~ModuleChange();

    ALISP_RAII_OBJECT(ModuleChange);

    const std::string &old_module() const { return m_prev_mod; }

  private:
    Environment &m_env;
    const std::string &m_prev_mod;
};

}  // namespace detail


}  // namespace env


}  // namespace alisp
