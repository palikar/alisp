#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_exception.hpp"

#include "alisp/utility/macros.hpp"

#include <fmt/format.h>


namespace alisp
{

namespace env
{

ALObjectPtr Environment::find(const ALObjectPtr t_sym)
{
        
    const auto name = t_sym->to_string();

    for (auto& scope : utility::reverse(m_stack.current_frame()))
    {
        if (scope.count(name)) { return scope.at(name); };
    }
        
    if (g_prime_values.count(name)) { return g_prime_values.at(name) ;}

    if (m_stack.root_scope().count(name)) { return m_stack.root_scope().at(name); };

    throw environment_error("\tUnbounded Symbol: " + name);
}

void Environment::update(const ALObjectPtr t_sym, ALObjectPtr t_value)
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

void Environment::put(const ALObjectPtr t_sym, ALObjectPtr t_val)
{
    auto& scope = m_stack.current_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Variable alredy exists: "  + name);}

    scope.insert({name, t_val});
}

void Environment::define_variable(const ALObjectPtr t_sym, ALObjectPtr t_value)
{
    auto& scope = m_stack.root_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Variable alredy exists: " + name);}

    scope.insert({name, t_value});
        
}

void Environment::define_function(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body)
{
        
    auto& scope = m_stack.root_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Function alredy exists: " + name);}

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
        
    scope.insert({name, new_fun});
    
}

void Environment::define_macro(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body)
{
        
    auto& scope = m_stack.root_scope();
    auto name = t_sym->to_string();

    if (scope.count(name)) { throw environment_error("Function alredy exists: " + name);}

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_macro_flag();
        
    scope.insert({name, new_fun});
    
}

void Environment::stack_dump() const
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

void Environment::callstack_dump() const
{

    using namespace fmt;
    using namespace std;

    cout << format("+{:-^48}+", "Call stack") << '\n';
    
    for (auto& [fun, prime] : utility::reverse(m_stack_trace) ) {


        if (prime) {
            cout << format("|{:<46}{:>}|", fun, "<-") << '\n';
        } else {
            cout << format("|{:<48}|", fun) << '\n';
        }

        
    }
    
    cout << format("+{:-^48}+", "") << '\n';
}

}

}
