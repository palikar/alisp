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
#include <cctype>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility/string_utils.hpp"

namespace alisp
{

ALObjectPtr Fstring_equals(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));

    return str_1->to_string().compare(str_2->to_string()) == 0 ? Qt : Qnil;
}


ALObjectPtr Fstring_contains(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));

    if (str_1->to_string().find(str_2->to_string()) != std::string::npos) { return Qt; }
    return Qnil;
}

ALObjectPtr Fstring_endswith(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));

    if (utility::ends_with(str_1->to_string(), str_2->to_string())) { return Qt; }
    return Qnil;
}

ALObjectPtr Fstring_startswith(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));

    if (utility::starts_with(str_1->to_string(), str_2->to_string())) { return Qt; }
    return Qnil;
}

ALObjectPtr Fstring_length(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str));
    return make_int(std::size(str->to_string()));
}

ALObjectPtr Fstring_capitalize(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str));
    auto &s = str->to_string();
    s[0]    = static_cast<char>(std::toupper(static_cast<int>(s[0])));
    return str;
}

ALObjectPtr Fchar_isalpha(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval->eval(obj->i(0));
    AL_CHECK(assert_char(str));
    return std::isalpha(static_cast<int>(str->to_int())) ? Qt : Qnil;
}

ALObjectPtr Fchar_isdigit(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval->eval(obj->i(0));
    AL_CHECK(assert_char(str));
    return std::isdigit(static_cast<int>(str->to_int())) ? Qt : Qnil;
}

ALObjectPtr Fstring_find(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));

    auto pos = str_1->to_string().find(str_2->to_string());
    if (pos != std::string::npos) { return make_int(pos); }
    return Qnil;
}

ALObjectPtr Fstring_replace(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    auto str_3 = eval->eval(obj->i(2));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));
    AL_CHECK(assert_string(str_3));
    return make_string(utility::replace(str_1->to_string(), str_2->to_string(), str_3->to_string()));
}

ALObjectPtr Fstring_replaceall(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    auto str_3 = eval->eval(obj->i(2));
    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));
    AL_CHECK(assert_string(str_3));

    return make_string(utility::replace_all(str_1->to_string(), str_2->to_string(), str_3->to_string()));
}

ALObjectPtr Fstring_split(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));

    AL_CHECK(assert_string(str_1));
    AL_CHECK(assert_string(str_2));


    return make_list(utility::split(str_1->to_string(), str_2->to_string()));
}

ALObjectPtr Fstring_substring(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto str   = eval->eval(obj->i(0));
    auto ind_1 = eval->eval(obj->i(1));
    auto ind_2 = eval->eval(obj->i(2));
    AL_CHECK(assert_string(str));
    AL_CHECK(assert_int(ind_1));
    AL_CHECK(assert_int(ind_2));
    return make_string(str->to_string().substr(static_cast<ALObject::string_type::size_type>(ind_1->to_int()),
                                               static_cast<ALObject::string_type::size_type>(ind_2->to_int())));
}

ALObjectPtr Fstring_splitlines(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str_1));
    return make_list(utility::split(str_1->to_string(), '\n'));
}

ALObjectPtr Fstring_upper(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str_1));
    return make_string(utility::str_toupper(str_1->to_string()));
}

ALObjectPtr Fstring_lower(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str_1));
    return make_string(utility::str_tolower(str_1->to_string()));
}

ALObjectPtr Fstring_strip(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval->eval(obj->i(0));
    AL_CHECK(assert_string(str_1));
    return make_string(utility::trim(str_1->to_string()));
}

ALObjectPtr Fstring_join(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(obj));
    std::string new_string{ "" };
    for (auto &t_obj : *obj)
    {
        auto e = eval->eval(t_obj);
        AL_CHECK(assert_string(e));
        new_string += e->to_string();
    }
    return make_string(new_string);
}

}  // namespace alisp
