#pragma once

#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"

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

struct CellStack {
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

    Scope& root_scope(){ return stacks.front().front(); }
    StackFrame& root_frame(){ return stacks.front() ;}

    StackFrame& current_frame(){ return stacks.back() ;}
    Scope& current_scope() { return stacks.back().back(); }

    Stack stacks;
};

}


class Environment
{

  public:
    static inline std::unordered_map<std::string, ALObjectPtr> g_symbol_table;
    static inline std::unordered_map<std::string, ALObjectPtr> g_global_symbol_table;
    static inline std::unordered_map<std::string, ALObjectPtr> g_prime_values;

  private:
    detail::CellStack m_stack;
    size_t m_call_depth = 0;

    std::vector<std::tuple<std::string, bool>> m_stack_trace; // name, is_prime    

  public:

    Environment() : m_stack()
    {
        m_call_depth = 0;
    }


    ALObjectPtr find(const ALObjectPtr t_sym);


    /**
     * Defines global variabe. This is used by defvar and defconst
     *
     * @param t_sym
     *
     * @return
     */
    void define_variable(const ALObjectPtr t_sym, ALObjectPtr t_value);

    void define_function(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body);
    
    void define_macro(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body);


    /**
     * Puts a local variable on the current scope. This is used by let and let*.
     *
     * @param t_sym
     * @param t_cell
     */
    void put(const ALObjectPtr t_sym, ALObjectPtr t_val);
    /** 
     * Used by setq to update the value of a cell
     *
     * @param t_sym 
     * @param t_value 
     */
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
        if (m_call_depth > MAX_FUNCTION_CALL_DEPTH) { throw std::runtime_error("Maximum function calldepth reached!"); }
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
    
    void dump() const;


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

}

}


}











 
