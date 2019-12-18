#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"

#include <fmt/format.h>


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

void Environment::dump() const
{
    using namespace fmt;
    using namespace std;

    cout << format("+{:-^48}+", "Environment") << '\n';

    size_t frame_index = 0;
    const auto stack_size = std::size(m_stack.stacks);
    
    for (auto& frame : m_stack.stacks) {
        cout << format("|{:^48}|", format("Frame {}", frame_index)) << '\n';
        cout << format("|{:^48}|", "") << '\n'; 

        cout << format("+{:-^10}+", "");
        cout << format("{:-^37}+", "") << '\n';

        size_t scope_index = 0;
        
        for (auto& scope : frame) {

            if (scope_index != 0) {
                cout << format("+{:-^10}+", "");
                cout << format("{:-^37}+", "") << '\n';
            }
            
            cout << format("|{:<10}|", format("Scope {}", scope_index));
            cout << format("{:<37}|", "") << '\n';
            
            cout << format("+{:-^10}+", "");
            cout << format("{:-^37}+", "") << '\n';

            for (auto& [sym, val] : scope) {
                cout << format("|{:<10}|", "");
                
                std::cout << format("{:<37}|", sym) << '\n';
            }

            ++scope_index;
        }
        
        if (frame_index != stack_size-1) {
            cout << format("+{:-^10}+", "");
            cout << format("{:-^37}+", "") << '\n';
        }
        
        ++frame_index;
    }
    

    cout << format("+{:-^10}+", "");
    cout << format("{:-^37}+", "") << '\n';
}


void callstack_dump() const {

    
}

}

}
