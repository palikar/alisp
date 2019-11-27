#pragma once


#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_macros.hpp"

// grep -h -R "DEFUN" ../src/  | grep "#define" -v | nl -v0 -n rn




namespace alisp::eval
{
class Evaluator;
}

namespace alisp {


class environment_error : public std::runtime_error
{
  public:
    environment_error(const std::string& t_why) : runtime_error(t_why){}

};


namespace env
{

inline std::unordered_map<std::string, ALObject> global_sym;
inline std::unordered_map<std::string, ALObject*> sym;

inline ALObject* intern(const std::string& name)
{
    
    if(global_sym.count(name))
    {
        return &global_sym.at(name);
        
    }

    if(sym.count(name))
    {
        return sym.at(name);
    }

    auto[new_sym, insertion] = sym.insert({name, make_symbol(name)});
    return new_sym->second;
}

namespace detail
{

struct CellStack {
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

    Stack stacks;
};

}


class Environment {

  public:
    inline static std::unordered_map<std::string, ALCell> prims;

  private:
    std::unordered_map<std::string, ALCell*> m_defs;
    detail::CellStack m_stack;
    size_t m_call_depth = 0;

  public:

    Environment() : m_defs()
    {
        m_call_depth = 0;
    }

    ALCell* find(const ALObject* t_sym)
    {
        auto name = t_sym->to_string();

        if (prims.count(name))
            return &prims.at(name);

        for (auto& scope : current_frame())
        {
            if (scope.count(name)) { return scope.at(name); };
        }

        if (m_defs.count(name)) { return m_defs.at(name); };

        throw environment_error("\tUnbounded Symbol: " + name);

        return nullptr;
    }

    void put(const ALObject* t_sym, ALCell* t_cell)
    {

        if(m_call_depth == 0 && std::size(m_stack.stacks) == 1) {
            if (current_frame().back().count(t_sym->to_string())) {
                m_defs.at(t_sym->to_string()) = t_cell;
            } else {
                m_defs.insert({t_sym->to_string(), t_cell});
            }
        } else {
            if (current_frame().back().count(t_sym->to_string())) {
                current_frame().back().at(t_sym->to_string()) = t_cell;
            } else {
                current_frame().back().insert({t_sym->to_string(), t_cell});
            }
        }
        
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

    auto call_depth() { return m_call_depth; }

    detail::CellStack::StackFrame& current_frame() {
        return m_stack.stacks.back();
    }

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




}
