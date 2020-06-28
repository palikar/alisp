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

#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/alisp/declarations/strings.hpp"

#include "alisp/utility/string_utils.hpp"

namespace alisp
{

ALObjectPtr Fstring_append(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    return make_string(str_1->to_string() += str_2->to_string());
}

ALObjectPtr Fstring_prepend(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    return make_string(str_2->to_string() += str_1->to_string());
}

ALObjectPtr Fstring_equals(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    return str_1->to_string().compare(str_2->to_string()) == 0 ? Qt : Qnil;
}

ALObjectPtr Fstring_less(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    return str_1->to_string().compare(str_2->to_string()) < 0 ? Qt : Qnil;
}

ALObjectPtr Fstring_contains(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    if (str_1->to_string().find(str_2->to_string()) != std::string::npos)
    {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_endswith(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    if (utility::ends_with(str_1->to_string(), str_2->to_string()))
    {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_startswith(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    if (utility::starts_with(str_1->to_string(), str_2->to_string()))
    {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_length(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval_check(eval, obj, 0, &assert_string<int>);

    return make_int(std::size(str->to_string()));
}

ALObjectPtr Fstring_capitalize(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval_check(eval, obj, 0, &assert_string<int>);

    auto &s = str->to_string();
    s[0]    = static_cast<char>(std::toupper(static_cast<int>(s[0])));
    return str;
}

ALObjectPtr Fchar_isalpha(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval_check(eval, obj, 0, &assert_char<int>);

    return std::isalpha(static_cast<int>(str->to_int())) ? Qt : Qnil;
}

ALObjectPtr Fchar_isdigit(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str = eval_check(eval, obj, 0, &assert_char<int>);

    return std::isdigit(static_cast<int>(str->to_int())) ? Qt : Qnil;
}

ALObjectPtr Fstring_find(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);


    auto pos = str_1->to_string().find(str_2->to_string());
    if (pos != std::string::npos)
    {
        return make_int(pos);
    }
    return Qnil;
}

ALObjectPtr Fstring_reverse(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);


    std::string copy = str_1->to_string();
    std::reverse(copy.begin(), copy.end());
    return make_string(copy);
}

ALObjectPtr Fstring_replace(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);

    auto str_3 = eval_check(eval, obj, 2, &assert_string<int>);

    return make_string(utility::replace(str_1->to_string(), str_2->to_string(), str_3->to_string()));
}

ALObjectPtr Fstring_replaceall(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);

    auto str_3 = eval_check(eval, obj, 2, &assert_string<int>);


    return make_string(utility::replace_all(str_1->to_string(), str_2->to_string(), str_3->to_string()));
}

ALObjectPtr Fstring_split(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));

    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    auto str_2 = eval_check(eval, obj, 1, &assert_string<int>);

    return make_list(utility::split(str_1->to_string(), str_2->to_string()));
}

ALObjectPtr Fstring_substring(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto str = eval_check(eval, obj, 0, &assert_string<int>);

    auto ind_1 = eval_check(eval, obj, 1, &assert_int<int>);

    auto ind_2 = eval_check(eval, obj, 2, &assert_int<int>);

    return make_string(str->to_string().substr(static_cast<ALObject::string_type::size_type>(ind_1->to_int()),
                                               static_cast<ALObject::string_type::size_type>(ind_2->to_int())));
}

ALObjectPtr Fstring_splitlines(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    return make_list(utility::split(str_1->to_string(), '\n'));
}

ALObjectPtr Fstring_upper(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    return make_string(utility::str_toupper(str_1->to_string()));
}

ALObjectPtr Fstring_lower(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    return make_string(utility::str_tolower(str_1->to_string()));
}

ALObjectPtr Fstring_strip(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto str_1 = eval_check(eval, obj, 0, &assert_string<int>);

    return make_string(utility::trim(str_1->to_string()));
}

ALObjectPtr Fstring_join(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
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
