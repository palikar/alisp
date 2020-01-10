#pragma once

#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_exception.hpp"

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

extern ALObjectPtr intern(const std::string& name);

namespace detail
{

struct CellStack
{
  public:
    using Scope = std::unordered_map<std::string, ALObjectPtr>;
    using StackFrame = std::vector<Scope>;
    using Stack = std::vector<StackFrame>;

    CellStack() {
        push_frame();
    }

    void push_frame() { stacks.emplace_back(1); }
    void pop_frame() { stacks.pop_back(); }

    void push_scope() { stacks.back().emplace_back(); }
    void pop_scope() { stacks.back().pop_back(); }

    // Scope& root_scope(){ return stacks.front().front(); }
    StackFrame& root_frame(){ return stacks.front() ;}

    StackFrame& current_frame(){ return stacks.back() ;}
    Scope& current_scope() { return stacks.back().back(); }

    Stack stacks;
};

}

class Module
{
  private:
    detail::CellStack::Scope m_root_scope;
    std::unordered_map<std::string, std::shared_ptr<Module>> m_modules;
    std::string m_name;

  public:

    Module(std::string t_name) : m_name(std::move(t_name)) {}

    detail::CellStack::Scope& root_scope() { return m_root_scope; }

    const std::string& name() { return m_name; }

    void add_module(std::shared_ptr<Module> t_module) { m_modules.insert({t_module->name(), t_module}); }

};


class Environment
{

  public:
    static inline std::unordered_map<std::string, ALObjectPtr> g_symbol_table;
    static inline std::unordered_map<std::string, ALObjectPtr> g_global_symbol_table;
    static inline std::unordered_map<std::string, ALObjectPtr> g_prime_values;

  private:
    detail::CellStack m_stack;
    std::unordered_map<std::string, std::shared_ptr<Module>> m_modules;
    std::shared_ptr<Module> m_active_module;

    size_t m_call_depth = 0;

    std::vector<std::tuple<std::string, bool>> m_stack_trace; // name, is_prime

  public:

    Environment() : m_stack()
      {
          m_call_depth = 0;
          m_active_module = (m_modules.insert({"--main--", std::make_shared<Module>("--main--")}).first->second);
      }

    // void load_module(ALObjectPtr t_sym, bool t_all, const std::string& t_file_name, const std::string& t_as, ALObjectPtr t_remap)
    // {
    //     // check if the module is loaded
    //     // read the file in
    //     // parse it
    //     // evaluate it
    //     // copy the wanted symbols to the current root scope
    // }
      
    void define_module(const std::string t_name)
    {
        auto new_mod = std::make_shared<Module>(t_name);
        m_active_module->add_module(new_mod);
        m_modules.insert({new_mod->name(), new_mod});
    }

    void activate_module(const std::string t_name)
    {
        m_active_module = m_modules.at(t_name);
    }

    const std::string& current_module()
    {
        return m_active_module->name();
    }
    
    bool module_loaded(const std::string& t_module_name)
    {
        return m_modules.count(t_module_name) != 0;
    }

    void import_root_scope(const std::string& t_from, const std::string& t_to)
    {
        m_modules.at(t_to) = m_modules.at(t_from);
    }

    ALObjectPtr find(const ALObjectPtr t_sym);

    void define_variable(const ALObjectPtr t_sym, ALObjectPtr t_value);

    void define_function(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body);

    void define_macro(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body);


    void put(const ALObjectPtr t_sym, ALObjectPtr t_val);

      
    void update(const ALObjectPtr t_sym, ALObjectPtr t_value);


    void new_scope()
    {
        m_stack.push_scope();
    }

    void destroy_scope()
    {
        m_stack.pop_scope();
    }

    void call_function()
    {
        ++m_call_depth;
        if (m_call_depth > MAX_FUNCTION_CALL_DEPTH) { throw environment_error("Maximum function calldepth reached!"); }
        m_stack.push_frame();
    }

    void finish_function()
    {
        --m_call_depth;
        m_stack.pop_frame();
    }

    size_t call_depth() { return m_call_depth; }

    bool in_function() { return m_call_depth != 0;}

    bool in_root() { return (!in_function()) && (std::size(m_stack.root_frame()) == 1); }

    void stack_dump() const;


    void callstack_dump() const;
    void trace_call(std::string t_trace, bool is_prime = false) { m_stack_trace.push_back({std::move(t_trace), is_prime}); }
    void trace_unwind() { if (!std::empty(m_stack_trace)) { m_stack_trace.pop_back(); } }
    auto get_stack_trace() -> auto& { return m_stack_trace; }

};


namespace detail
{

struct CallTracer
{
  private:
    std::string m_function;
    std::string m_args;
    bool m_is_prime = false;

  public:

    explicit CallTracer(Environment& t_env) : m_env(t_env) { }

    ~CallTracer() {
        m_env.trace_unwind();
    }


    void function_name (std::string t_func, bool t_is_prime = false) {
        m_is_prime = t_is_prime;
        m_function = std::move(t_func);
        m_env.trace_call("(" + m_function + ")", t_is_prime);
    }

    void dump() {

        if ( ALISP_UNLIKELY( !std::empty(m_env.get_stack_trace()) )) {
            m_env.callstack_dump();
            m_env.get_stack_trace().clear();
        } else {
            return;
        }

    }



    CallTracer(CallTracer &&) = default;
    CallTracer& operator=(CallTracer &&) = default;
    CallTracer(const CallTracer &) = delete;
    CallTracer& operator=(const CallTracer &) = delete;

    // TODO: trace here

  private:
    Environment& m_env;

};

struct FunctionCall
{
  public:

    explicit FunctionCall(Environment& t_env) : m_env(t_env) {m_env.call_function();}
    ~FunctionCall() {m_env.finish_function();}

    FunctionCall(FunctionCall &&) = default;
    FunctionCall& operator=(FunctionCall &&) = default;
    FunctionCall(const FunctionCall &) = delete;
    FunctionCall& operator=(const FunctionCall &) = delete;

    // TODO: trace here

  private:
    Environment& m_env;

};

struct ScopePushPop
{
  public:

    explicit ScopePushPop(Environment& t_env) : m_env(t_env) {m_env.new_scope();}
    ~ScopePushPop() {m_env.destroy_scope();}

    ScopePushPop(ScopePushPop &&) = default;
    ScopePushPop& operator=(ScopePushPop &&) = default;
    ScopePushPop(const ScopePushPop &) = delete;
    ScopePushPop& operator=(const ScopePushPop  &) = delete;

  private:
    Environment& m_env;
};

struct ModuleChange
{
  private:
    
    Environment& m_env;
    const std::string& m_prev_mod;
    
  public:

    ModuleChange(Environment& t_env, const std::string& t_module) :
        m_env(t_env), m_prev_mod(m_env.current_module()) { m_env.activate_module(t_module); }
    
    ~ModuleChange() {m_env.activate_module(m_prev_mod);}
    ModuleChange(ModuleChange &&) = default;
    ModuleChange& operator=(ModuleChange &&) = default;
    ModuleChange(const ModuleChange &) = delete;
    ModuleChange& operator=(const ModuleChange  &) = delete;

    const std::string& old_module() const { return m_prev_mod; } 

};

}

}


}
