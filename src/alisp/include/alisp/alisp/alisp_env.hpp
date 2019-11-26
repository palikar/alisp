#pragma once


#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"

// grep -h -R "DEFUN" ../src/  | grep "#define" -v | nl -v0 -n rn

namespace alisp::eval
{
class Evaluator;
}

#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define DEFSYM(var, sym_name)                                           \
    inline auto var = &env::global_sym.insert({sym_name, ALObject(sym_name, true)}).first->second

#define DEFVAR(var, sym_name)                                           \
    inline auto var = &env::global_sym.insert({sym_name, ALObject(sym_name, true)}).first->second; \
    inline auto V_ ## var = env::Environment::prims.insert({sym_name, ALCell(sym_name).make_value( var )})


#define DEFUN(name, sym)                                           \
    extern ALObject* F##name (ALObject*, env::Environment*, eval::Evaluator*); \
    inline auto Q##name = &env::global_sym.insert({sym, ALObject(sym, true)}).first->second; \
    inline auto P##name = env::Environment::prims.insert({sym, ALCell(sym).make_prim(&F##name)})



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
    void push_scope() { stacks.back().emplace_back(); }

    void pop_frame() { stacks.pop_back(); }
    void pop_scope() { stacks.back().pop_back(); }

    Stack stacks;
    int call_depth = 0;

};

}


class Environment {

  public:
    inline static std::unordered_map<std::string, ALCell> prims;

  private:
    std::unordered_map<std::string, ALCell*> defs;

  public:

    Environment() : defs()
    {
        auto ver = new ALCell("version");
        ver->make_value(make_string("1.0.0"));
        defs.insert({"version", ver});
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

        throw environment_error("\tUnbounded Symbol: " + name);
        
        return nullptr;
    }

    void put(const ALObject* t_sym, ALCell* t_cell)
    {
        if (current_frame().back().count(t_sym->to_string())) {
            current_frame().back().at(t_sym->to_string()) = t_cell;
        } else {
            current_frame().back().insert({t_sym->to_string(), t_cell});
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
        ++call_depth;
        m_stack.push_frame();
    }

    void finish_function()
    {
        --call_depth;
        m_stack.pop_frame();
    }

    detail::CellStack::StackFrame& current_frame() {
        return m_stack.stacks.back();
    }


  private:
    detail::CellStack m_stack;
    size_t call_depth = 0;
};
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
