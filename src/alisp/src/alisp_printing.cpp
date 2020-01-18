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
#include "alisp/alisp/alisp_pattern_matching.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{

ALObjectPtr Fprint(ALObjectPtr t_obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(t_obj);

    for (auto child : *t_obj)
    {
        auto val = eval->eval(child);
        
        make_visit(val,
                   type(ALObjectType::INT_VALUE ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_int(); },
                   type(ALObjectType::REAL_VALUE ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_real(); },
                   type(ALObjectType::STRING_VALUE ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_string(); },
                   type(ALObjectType::SYMBOL ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_string(); }
            );
        
    }    
    
    return Qt;

}

ALObjectPtr Fprintln(ALObjectPtr t_obj, env::Environment* env, eval::Evaluator* eval)
{
    Fprint(t_obj, env, eval);
    std::cout << '\n';
    return Qt;
}

ALObjectPtr Fdump(ALObjectPtr t_obj, env::Environment*, eval::Evaluator* eval)
{
    std::cout << dump(eval->eval(t_obj->i(0))) << "\n";
    return Qt;
}

ALObjectPtr Fdumpstack(ALObjectPtr, env::Environment* env, eval::Evaluator*)
{
    env->stack_dump();
    return Qt;
}

ALObjectPtr Fdumpcallstack(ALObjectPtr, env::Environment* env, eval::Evaluator*)
{
    env->callstack_dump();
    return Qt;
}

ALObjectPtr Fdumplicense(ALObjectPtr, env::Environment*, eval::Evaluator*)
{
    std::cout << AL_LICENSE_TEXT << '\n';
    return Qt;
}

ALObjectPtr Fdumpcredits(ALObjectPtr, env::Environment*, eval::Evaluator*)
{
    std::cout << AL_CREDITS_TEXT << '\n';
    return Qt;
}

}
