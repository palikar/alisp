#pragma once


#include <unordered_map>
#include <vector>
#include <array>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"


namespace alisp {

namespace env{


extern std::unordered_map<std::string, ALObject> global_sym;
extern std::unordered_map<std::string, ALObject> global_cell;
extern std::unordered_map<std::string, ALObject*> sym;

extern ALObject* qnil;
extern ALObject* t;


ALObject* intern(const std::string& name);


struct CellStack {
    using Scope = std::unordered_map<std::string, ALCell>;
    using StackFrame = std::vector<Scope>;
    using Stacks = std::vector<StackFrame>;

    CellStack() {
        push_frame();
        push_scope();
    }

    void push_frame() { stacks.emplace_back(1); }
    void push_scope() { stacks.back().emplace_back(); }

    void pop_frame() { stacks.pop_back(); }
    void pop_scope() { stacks.back().pop_back(); }

    Stacks stacks;
    int call_depth = 0;

};

class Environment {

  public:
    static std::unordered_map<std::string, ALCell> prims;
    
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

        if (defs.count(name))
            return defs.at(name);

        return nullptr;
    }

    void put(const ALObject* t_sym, ALCell* t_cell)
    {
        
        defs.insert({t_sym->to_string(), t_cell});
    }


  private:
    CellStack m_stack;
};

}

}
