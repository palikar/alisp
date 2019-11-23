#include "alisp/alisp/alisp_env.hpp"



namespace alisp
{
namespace env{



ALObject* setq(ALObject* obj, Environment* env)
{
    
    auto new_var = new ALCell(obj->i(1)->to_string());
    new_var->make_value(obj->i(2));
    env->put(obj->i(1), new_var);    
    return t;
}


ALObject* print(ALObject* obj, Environment*)
{

    std::cout <<ALObject::dump(obj->i(1)) << "\n";

    return t;
}


std::unordered_map<std::string, ALObject> global_sym = {
    // simple constants
    {"nil", ALObject("nil", true)},
    {"t", ALObject("t", true)},

    // language constructs
    {"if", ALObject("if", true)},
    {"while", ALObject("while", true)},
    {"defun", ALObject("defun", true)},
    {"defvar", ALObject("defvar", true)},
    {"setq", ALObject("setq", true)},
    
};
std::unordered_map<std::string, ALObject*> sym;

ALObject* qnil = &global_sym.at("nil");
ALObject* t = &global_sym.at("t");

ALObject* intern(const std::string& name)
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


std::unordered_map<std::string, ALCell> Environment::prims = {};
auto Qsetq = Environment::prims.insert({"setq", ALCell("setq").make_prim(&setq)});
auto Qprint = Environment::prims.insert({"print", ALCell("print").make_prim(&print)});



}

}
