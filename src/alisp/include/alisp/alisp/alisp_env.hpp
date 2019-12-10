#pragma once


#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"


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
    using Scope = std::unordered_map<std::string, ALCell*>;
    using StackFrame = std::vector<Scope>;
    using Stack = std::vector<StackFrame>;

    CellStack() {
        push_frame();
        push_scope();
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


class Environment {

  public:
    static inline std::unordered_map<std::string, ALObject> g_global_symbol_table;
    static inline std::unordered_map<std::string, ALObject*> g_symbol_table;
    static inline std::unordered_map<std::string, ALCell> g_prime_values;

  private:
    detail::CellStack m_stack;
    size_t m_call_depth = 0;

  public:

    Environment() : m_stack()
    {
        m_call_depth = 0;
    }


    ALCell* find(const ALObject* t_sym)
    {
        const auto name = t_sym->to_string();

        for (auto& scope : current_frame())
        {
            if (scope.count(name)) { return scope.at(name); };
        }
        
        if (g_prime_values.count(name)) { return &g_prime_values.at(name) ;}

        if (m_stack.root_scope().count(name)) { return m_stack.root_scope().at(name); };

        throw environment_error("\tUnbounded Symbol: " + name);

        return nullptr;
    }


    /**
     * Defines global variabe. This is used by defvar and defconst
     *
     * @param t_sym
     *
     * @return
     */
    void define_variable(const ALObject* t_sym, ALObject* t_value)
    {
        auto& scope = m_stack.root_scope();
        auto name = t_sym->to_string();

        if (scope.count(name)) { throw environment_error("Variable alredy exists");}

        auto new_cell = new ALCell(name);
        new_cell->make_value(t_value);
        scope.insert({name, new_cell});
    }

    void define_function(const ALObject* t_sym, ALObject* t_params, ALObject* t_body)
    {
        
        auto& scope = m_stack.root_scope();
        auto name = t_sym->to_string();

        if (scope.count(name)) { throw environment_error("Function alredy exists");}

        auto new_cell = new ALCell(name);
        new_cell->make_function(t_params, t_body);
        scope.insert({name, new_cell});
    
    }

    /**
     * Puts a local variable on the current scope. This is used by let and let*.
     *
     * @param t_sym
     * @param t_cell
     */
    void put(const ALObject* t_sym, ALCell* t_cell)
    {
        auto& scope = m_stack.current_scope();
        auto name = t_sym->to_string();

        if (scope.count(name)) { throw environment_error("Variable alredy exists");}

        scope.insert({name, t_cell});
    }

    /** 
     * Used by setq to update the value of a cell
     *
     * @param t_sym 
     * @param t_value 
     */
    void update(const ALObject* t_sym, ALObject* t_value)
    {
        find(t_sym)->make_value(t_value);
    }


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

    bool in_root() { return !in_function() and std::size(m_stack.root_frame()) == 0;}

    
    detail::CellStack::StackFrame& current_frame() {
        return m_stack.stacks.back();
    }

    // TODO: trace here
    
    
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

DEFVAR(Qt, "t");
DEFVAR(Qnil, "nil");

DEFSYM(Qoptional, "&optional");
DEFSYM(Qrest, "&rest");

DEFUN(defun, "defun");
DEFUN(defvar, "defvar");

DEFUN(setq, "setq");

DEFUN(print, "print");
DEFUN(println, "println");

DEFUN(quote, "quote");
DEFUN(if, "if");
DEFUN(while, "while");
DEFUN(progn, "progn");
DEFUN(letx, "let*");
DEFUN(let, "let");

DEFUN(plus, "+");
DEFUN(minus, "-");
DEFUN(dev, "/");
DEFUN(multiply, "*");

DEFUN(gt, ">");
DEFUN(geq, ">=");
DEFUN(lt, "<");
DEFUN(leq, "<=");
DEFUN(eq, "==");
DEFUN(neq, "!=");

DEFUN(exit, "exit");

DEFUN(dump, "dump");




}
