#pragma once

#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"

#include "alisp/utility/helpers.hpp"


namespace alisp::eval
{
class Evaluator;
}

namespace alisp
{


class environment_error : public std::runtime_error
{
  public:
    environment_error(const std::string& t_why) : runtime_error(t_why){}

};


namespace env
{

extern ALObject* intern(const std::string& name);

namespace detail
{

struct CellStack {
  public:
    using Scope = std::unordered_map<std::string, ALObject*>;
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
    static inline std::unordered_map<std::string, ALObject*> g_symbol_table;
    static inline std::unordered_map<std::string, ALObject> g_global_symbol_table;
    static inline std::unordered_map<std::string, ALObject> g_prime_values;

  private:
    detail::CellStack m_stack;
    std::vector<std::string> m_stack_trace;
    size_t m_call_depth = 0;

    // auto scan(const ALObject* t_sym)
    // {}


  public:

    Environment() : m_stack()
    {
        m_call_depth = 0;
    }


    ALObject* find(const ALObject* t_sym);


    /**
     * Defines global variabe. This is used by defvar and defconst
     *
     * @param t_sym
     *
     * @return
     */
    void define_variable(const ALObject* t_sym, ALObject* t_value);

    void define_function(const ALObject* t_sym, ALObject* t_params, ALObject* t_body);
    
    void define_macro(const ALObject* t_sym, ALObject* t_params, ALObject* t_body);


    /**
     * Puts a local variable on the current scope. This is used by let and let*.
     *
     * @param t_sym
     * @param t_cell
     */
    void put(const ALObject* t_sym, ALObject* t_val);
    /** 
     * Used by setq to update the value of a cell
     *
     * @param t_sym 
     * @param t_value 
     */
    void update(const ALObject* t_sym, ALObject* t_value);


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

    auto& get_stack_trace() { return m_stack_trace; }

    void dump() const;


};


namespace detail
{

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











 
