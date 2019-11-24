#include "alisp/alisp/alisp_env.hpp"
#include "alisp/utility/macros.hpp"



namespace alisp
{
namespace env{



ALObject* defun(ALObject* obj, Environment* env)
{
    auto new_fun = new ALCell(obj->i(0)->to_string());
    new_fun->make_function(obj->i(1), splice(obj, 2));
    env->put(obj->i(0), new_fun);
    return t;
}

ALObject* setq(ALObject* obj, Environment* env)
{
    auto new_var = new ALCell(obj->i(0)->to_string());
    new_var->make_value(obj->i(1));
    env->put(obj->i(0), new_var);
    return t;
}


ALObject* print(ALObject* obj, Environment* env)
{
    

    const auto fun =
        [](ALObject* obj){
            
            if(obj->type() == ALObjectType::STRING_VALUE) {
                std::cout << obj->to_string() << "\n";
                return t;
            } else if(obj->type() == ALObjectType::INT_VALUE) {                
                std::cout << obj->to_int() << "\n";
                return t;
            }  else if(obj->type() == ALObjectType::REAL_VALUE) {
                std::cout << obj->to_real() << "\n";
                return t;
            }
            return qnil;
        };
    
    if(obj->i(0)->type() == ALObjectType::SYMBOL) {
        auto ob = env->find(obj->i(0));
        // TODO: check here
        return fun(ob->value());
    }

    if(obj->i(0)->type() == ALObjectType::LIST) {
        return qnil;
    }

    return fun(obj->i(0));
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
auto Qdefun = Environment::prims.insert({"defun", ALCell("defun").make_prim(&defun)});



}

}
