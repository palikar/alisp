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
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_streams.hpp"

#include "alisp/alisp/declarations/printing.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


ALObjectPtr Fprint(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(t_obj));

    for (auto child : *t_obj)
    {
        auto val = eval->eval(child);

        make_visit(
          val,
          type(ALObjectType::INT_VALUE) >>= [](ALObjectPtr obj) { al::cout << obj->to_int(); },
          type(ALObjectType::REAL_VALUE) >>= [](ALObjectPtr obj) { al::cout << obj->to_real(); },
          type(ALObjectType::STRING_VALUE) >>= [](ALObjectPtr obj) { al::cout << obj->to_string(); },
          type(ALObjectType::SYMBOL) >>= [](ALObjectPtr obj) { al::cout << obj->to_string(); });
    }

    return Qt;
}

ALObjectPtr Feprint(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(t_obj));

    for (auto child : *t_obj)
    {
        auto val = eval->eval(child);

        make_visit(
          val,
          type(ALObjectType::INT_VALUE) >>= [](ALObjectPtr obj) { al::cerr << obj->to_int(); },
          type(ALObjectType::REAL_VALUE) >>= [](ALObjectPtr obj) { al::cerr << obj->to_real(); },
          type(ALObjectType::STRING_VALUE) >>= [](ALObjectPtr obj) { al::cerr << obj->to_string(); },
          type(ALObjectType::SYMBOL) >>= [](ALObjectPtr obj) { al::cerr << obj->to_string(); });
    }

    return Qt;
}

ALObjectPtr Fprintln(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
{
    Fprint(t_obj, env, eval);
    al::cout << '\n';
    return Qt;
}

ALObjectPtr Feprintln(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
{
    Feprint(t_obj, env, eval);
    al::cerr << '\n';
    return Qt;
}

ALObjectPtr Fdump(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    std::cout << dump(eval->eval(t_obj->i(0))) << "\n";
    return Qt;
}

ALObjectPtr Fdumpstack(const ALObjectPtr&, env::Environment *env, eval::Evaluator *)
{
    env->stack_dump();
    return Qt;
}

ALObjectPtr Fdumpcallstack(const ALObjectPtr&, env::Environment *env, eval::Evaluator *)
{
    env->callstack_dump();
    return Qt;
}

ALObjectPtr Fdumpsystem(const ALObjectPtr&, env::Environment *env, eval::Evaluator *)
{
    env->env_dump();
    return Qt;
}

ALObjectPtr Fdumplicense(const ALObjectPtr&, env::Environment *, eval::Evaluator *)
{
    al::cout << AL_LICENSE_TEXT << '\n';
    return Qt;
}

ALObjectPtr Fdumpcredits(const ALObjectPtr&, env::Environment *, eval::Evaluator *)
{
    al::cout << AL_CREDITS_TEXT << '\n';
    return Qt;
}

ALObjectPtr Fread_line(const ALObjectPtr&, env::Environment *, eval::Evaluator *)
{
    std::string line;
    return make_string(al::cin.get().get_line());
}

ALObjectPtr Fread_char(const ALObjectPtr&, env::Environment *, eval::Evaluator *)
{
    return make_char(al::cin.get().get_char());
}

}  // namespace alisp
