#pragma once


#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_declarations.hpp"


namespace alisp
{

inline auto module_init(std::string t_name) {
    return std::make_shared<env::Module>(std::move(t_name));
}

inline void module_defun(env::Module* t_module, const std::string& t_name, Prim::func_type fun) {
    auto& new_fun = t_module->get_root().insert({t_name, make_prime(fun, t_name)}).first->second;
    new_fun->set_function_flag();
    new_fun->set_prop("--module--", make_string(t_module->name()));          
}

inline void module_defvar(env::Module* t_module, const std::string& t_name, ALObjectPtr val) {
    auto& new_var = t_module->get_root().insert({t_name, val}).first->second;
    new_var->set_prop("--module--", make_string(t_module->name()));
}

inline void module_defconst(env::Module* t_module, const std::string& t_name, ALObjectPtr val) {
    auto& new_var = t_module->get_root().insert({t_name, val}).first->second;
    new_var->set_const_flag();
    new_var->set_prop("--module--", make_string(t_module->name()));
}


inline void module_dump(env::Module* t_module) {
    for (auto& [name, sym]  : t_module->root_scope()) {
        std::cout << name << " : " << sym << "\n";
    }
}






}

