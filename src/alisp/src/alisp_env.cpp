/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */





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

    if (m_active_module->root_scope().count(name)) { return m_active_module->root_scope().at(name); };

    throw environment_error("\tUnbounded Symbol: " + name);
}

void Environment::update(const ALObjectPtr t_sym, ALObjectPtr t_value)
{
    const auto name = t_sym->to_string();

    for (auto& scope : m_stack.current_frame())
    {
        if (scope.count(name)) {
            auto& sym = scope.at(name);
            if (sym->check_const_flag()) {
                throw environment_error("\tTrying to change a const symbol: " + name);
            }
            sym = t_value;
            return;
        };
    }
        
    if (m_active_module->root_scope().count(name)) {
        auto& sym = m_active_module->root_scope().at(name);
        if (sym->check_const_flag()) {
            throw environment_error("\tTrying to change a const symbol: " + name);
        }
        sym = t_value;
        return;
    };

    throw environment_error("\tUnbounded Symbol: " + name);
}

void Environment::put(const ALObjectPtr t_sym, ALObjectPtr t_val)
{
    auto& scope = m_stack.current_scope();
    auto name = t_sym->to_string();

    NameValidator::validate_object_name(name);

    if (scope.count(name)) { throw environment_error("Variable alredy exists: "  + name);}

    scope.insert({name, t_val});
}

void Environment::define_variable(const ALObjectPtr t_sym, ALObjectPtr t_value)
{
    auto& scope = m_active_module->root_scope();
    auto name = t_sym->to_string();

    NameValidator::validate_object_name(name);

    if (scope.count(name)) { throw environment_error("Variable alredy exists: " + name);}

    t_value->set_prop("--module--", make_string(m_active_module->name()));

    scope.insert({name, t_value});
        
}

void Environment::define_function(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body)
{
        
    auto& scope = m_active_module->root_scope();
    auto name = t_sym->to_string();

    NameValidator::validate_object_name(name);

    if (scope.count(name)) { throw environment_error("Function alredy exists: " + name);}

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_prop("--module--", make_string(m_active_module->name()));
    new_fun->set_prop("--name--", make_string(name));
        
    scope.insert({name, new_fun});
    
}

void Environment::define_macro(const ALObjectPtr t_sym, ALObjectPtr t_params, ALObjectPtr t_body)
{
        
    auto& scope = m_active_module->root_scope();
    auto name = t_sym->to_string();

    NameValidator::validate_object_name(name);

    if (scope.count(name)) { throw environment_error("Function alredy exists: " + name);}

    auto new_fun = make_object(t_params, t_body);
    new_fun->set_function_flag();
    new_fun->set_macro_flag();
    new_fun->set_prop("--module--", make_string(m_active_module->name()));
    new_fun->set_prop("--name--", make_string(name));
        
    scope.insert({name, new_fun});
    
}

void Environment::activate_module(const std::string& t_name)
{
    Vcurrent_module->set(t_name);
    m_active_module = m_modules.at(t_name).get();
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
