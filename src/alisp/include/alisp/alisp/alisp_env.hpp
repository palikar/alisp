#pragma once


#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"

// grep -h -R "DEFUN" ../src/  | grep "#define" -v | nl -v0 -n rn

#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define DEFSYM(var, sym_name)                                           \
    inline auto var = &env::global_sym.insert({sym_name, ALObject(sym_name, true)}).first->second

#define DEFVAR(var, sym_name)                                           \
    inline auto var = &env::global_sym.insert({sym_name, ALObject(sym_name, true)}).first->second; \
    inline auto V_ ## var = env::Environment::prims.insert({sym_name, ALCell(sym_name).make_value( var )})


#define DEFUN(name)                                                     \
    extern ALObject* F##name (ALObject*, env::Environment*);            \
    inline auto Q##name = &env::global_sym.insert({#name, ALObject(#name, true)}).first->second; \
    inline auto P##name = env::Environment::prims.insert({#name, ALCell(#name).make_prim(&F##name)})



namespace alisp {

namespace env{

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

        // TODO: Unbound symbol; throw here
        
        return nullptr;
    }

    void put(const ALObject* t_sym, ALCell* t_cell)
    {
        current_frame().back().insert({t_sym->to_string(), t_cell});
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

    CellStack::StackFrame& current_frame() {
        return m_stack.stacks.back();
    }


  private:
    CellStack m_stack;
    size_t call_depth = 0;
};
}


DEFVAR(Qt, "t");
DEFVAR(Qnil, "nil");

DEFSYM(Qoptional, "&optional");
DEFSYM(Qrest, "&rest");


DEFUN(print);
DEFUN(setq);
DEFUN(quote);
DEFUN(defun);

DEFUN(if);



}
