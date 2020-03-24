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
#include <string>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{


ALObjectPtr Fprop_set(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));

    auto target = eval->eval(obj->i(0));

    auto prop = eval->eval(obj->i(1));
    AL_CHECK(assert_string(prop));
    auto &prop_name = prop->to_string();

    // if (!target->prop_exists(prop_name)) { return Qnil; }

    target->set_prop(prop_name, eval->eval(obj->i(2)));

    return Qt;
}

ALObjectPtr Fprop_get(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    auto target = eval->eval(obj->i(0));

    auto prop = eval->eval(obj->i(1));
    AL_CHECK(assert_string(prop));
    auto &prop_name = prop->to_string();

    if (!target->prop_exists(prop_name))
    {
        return Qnil;
    }

    return target->get_prop(prop_name);
}


ALObjectPtr Fprop_exists(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    auto target = eval->eval(obj->i(0));

    auto prop = eval->eval(obj->i(1));
    AL_CHECK(assert_string(prop));
    auto &prop_name = prop->to_string();

    return target->prop_exists(prop_name) ? Qt : Qnil;
}

ALObjectPtr Fprop_list(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));

    auto target = eval->eval(obj->i(0));
    ALObject::list_type props;
    for (auto &[name, _] : target->props())
    {
        props.push_back(make_string(name));
    }
    return make_object(props);
}

}  // namespace alisp
