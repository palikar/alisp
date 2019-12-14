#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{

namespace env
{

ALObject* Environment::find(const ALObject* t_sym)
{
        
    const auto name = t_sym->to_string();

    for (auto& scope : utility::reverse(m_stack.current_frame()))
    {
        if (scope.count(name)) { return scope.at(name); };
    }
        
    if (g_prime_values.count(name)) { return &g_prime_values.at(name) ;}

    if (m_stack.root_scope().count(name)) { return m_stack.root_scope().at(name); };

    throw environment_error("\tUnbounded Symbol: " + name);
}

void Environment::update(const ALObject* t_sym, ALObject* t_value)
{
    const auto name = t_sym->to_string();

    for (auto& scope : m_stack.current_frame())
    {
        if (scope.count(name)) {
            scope.at(name) = t_value;
            return;
        };
    }
        
    if (m_stack.root_scope().count(name)) {
        m_stack.root_scope().at(name) = t_value;
        return;
    };

    throw environment_error("\tUnbounded Symbol: " + name);
}

void Environment::put(const ALObject* t_sym, ALObject* t_val)
{
    auto& scope = m_stack.current_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Variable alredy exists");}

    scope.insert({name, t_val});
}

void Environment::define_variable(const ALObject* t_sym, ALObject* t_value)
{
    auto& scope = m_stack.root_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Variable alredy exists");}

    scope.insert({name, t_value});
        
}

void Environment::define_function(const ALObject* t_sym, ALObject* t_params, ALObject* t_body)
{
        
    auto& scope = m_stack.root_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Function alredy exists");}

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
        
    scope.insert({name, new_fun});
    
}

void Environment::define_macro(const ALObject* t_sym, ALObject* t_params, ALObject* t_body)
{
        
    auto& scope = m_stack.root_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Function alredy exists");}

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_macro_flag();
        
    scope.insert({name, new_fun});
    
}

}

}
