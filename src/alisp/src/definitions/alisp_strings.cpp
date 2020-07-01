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


struct string_append
{

    inline static const std::string name = "string-append";

    inline static const std::string doc{ R"((string-append STRING1 STRING2)

Return a new string by concatenatig `STRING1` to `STRING2`.
)" };

    static ALObjectPtr Fstring_append(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        return make_string(str_1->to_string() += str_2->to_string());
    }
};

struct string_prepend
{
    inline static const std::string name = "string-prepend";

    inline static const std::string doc{ R"((string-prepend STRING1 STRING2)

Return a new string by prepending `STRING1` to `STRING2`.
)" };

    static ALObjectPtr Fstring_prepend(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        return make_string(str_2->to_string() += str_1->to_string());
    }
};

struct string_equals
{
    inline static const std::string name = "string-equals";

    inline static const std::string doc{ R"((string-equals STRING1 STRING2)

Return `t` if the proviced strings equal lexicographically. Return `nil` otherwise.
)" };

    static ALObjectPtr Fstring_equals(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        return str_1->to_string().compare(str_2->to_string()) == 0 ? Qt : Qnil;
    }
};

struct string_less
{
    inline static const std::string name = "string-less";

    inline static const std::string doc{ R"((string-less STRING1 STRING2))" };

    static ALObjectPtr Fstring_less(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        return str_1->to_string().compare(str_2->to_string()) < 0 ? Qt : Qnil;
    }
};

struct string_contains
{
    inline static const std::string name = "string-contains";

    inline static const std::string doc{ R"((string-contains STRING SUBSTRING)

Return `t` if `STRING` contains `SUBSTRING` as a substring. Return `nil` otherwise.
)" };

    static ALObjectPtr Fstring_contains(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        if (str_1->to_string().find(str_2->to_string()) != std::string::npos)
        {
            return Qt;
        }
        return Qnil;
    }
};

struct string_endswith
{
    inline static const std::string name = "string-endswith";

    inline static const std::string doc{ R"((string-contains STRING SUFFIX)

Return `t` if `STRING` ends with `SUFFIX`. Return `nil` otherwise.
)" };

    static ALObjectPtr Fstring_endswith(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        if (utility::ends_with(str_1->to_string(), str_2->to_string()))
        {
            return Qt;
        }
        return Qnil;
    }
};

struct string_startswith
{
    inline static const std::string name = "string-startswith";

    inline static const std::string doc{ R"((string-contains STRING PREFIX)

Return `t` if `STRING` starts with `PREFIX`. Return `nil` otherwise.
)" };

    static ALObjectPtr Fstring_startswith(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        if (utility::starts_with(str_1->to_string(), str_2->to_string()))
        {
            return Qt;
        }
        return Qnil;
    }
};

struct string_length
{
    inline static const std::string name = "string-length";

    inline static const std::string doc{ R"((string-length STRING)

Return the length of the provided string.
)" };

    static ALObjectPtr Fstring_length(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str = eval_check(eval, obj, 0, &assert_string<size_t>);

        return make_int(std::size(str->to_string()));
    }
};

struct string_capitalize
{
    inline static const std::string name = "string-capitalize";

    inline static const std::string doc{ R"((string-capitalize STRING)

Capitalized the first letter of the provided string.
)" };

    static ALObjectPtr Fstring_capitalize(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto &s = str->to_string();
        s[0]    = static_cast<char>(std::toupper(static_cast<size_t>(s[0])));
        return str;
    }
};

struct char_isalpha
{
    inline static const std::string name = "char-isalpha";

    inline static const std::string doc{ R"((char-isalpha CHAR)

Check if the character CHAR is a letter.
)" };

    static ALObjectPtr Fchar_isalpha(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str = eval_check(eval, obj, 0, &assert_char<size_t>);

        return std::isalpha(static_cast<int>(str->to_int())) ? Qt : Qnil;
    }
};

struct char_isdigit
{
    inline static const std::string name = "char-isdigit";

    inline static const std::string doc{ R"((char-isdigit CHAR)

Check if the character CHAR is a digit.
)" };

    static ALObjectPtr Fchar_isdigit(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str = eval_check(eval, obj, 0, &assert_char<size_t>);

        return std::isdigit(static_cast<int>(str->to_int())) ? Qt : Qnil;
    }
};

struct string_find
{
    inline static const std::string name = "string-find";

    inline static const std::string doc{ R"((string-find STRING SUBSTRING)

Return the first index where `SUBSTRING` is contained in `STRINGE`.
)" };

    static ALObjectPtr Fstring_find(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);


        auto pos = str_1->to_string().find(str_2->to_string());
        if (pos != std::string::npos)
        {
            return make_int(pos);
        }
        return Qnil;
    }
};

struct string_reverse
{
    inline static const std::string name = "string-reverse";

    inline static const std::string doc{ R"((string-reverse STRING)

Rerturn a new string with the elements of STRING in reverse order
)" };

    static ALObjectPtr Fstring_reverse(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);


        std::string copy = str_1->to_string();
        std::reverse(copy.begin(), copy.end());
        return make_string(copy);
    }
};

struct string_replace
{
    inline static const std::string name = "string-replace";

    inline static const std::string doc{ R"((string-replace STRING SUBSTRING NEWSTRING)

Replace one occurrence of `SUBSTRING` in STRING with `NEWSTRING`. The
new string is returned.
)" };

    static ALObjectPtr Fstring_replace(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<3>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);

        auto str_3 = eval_check(eval, obj, 2, &assert_string<size_t>);

        return make_string(utility::replace(str_1->to_string(), str_2->to_string(), str_3->to_string()));
    }
};

struct string_replaceall
{
    inline static const std::string name = "string-replaceall";

    inline static const std::string doc{ R"((string-replaceall STRING SUBSTRING NEWSTRING)

Replace all occurrences of `SUBSTRING` in STRING with `NEWSTRING`. The
new string is returned.
)" };

    static ALObjectPtr Fstring_replaceall(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<3>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);

        auto str_3 = eval_check(eval, obj, 2, &assert_string<size_t>);


        return make_string(utility::replace_all(str_1->to_string(), str_2->to_string(), str_3->to_string()));
    }
};

struct string_split
{
    inline static const std::string name = "string-split";

    inline static const std::string doc{ R"((string-split STRING DELIMETER)

Split `STRING` based on `DELIMETER` and return a list of the parts.
)" };

    static ALObjectPtr Fstring_split(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto str_2 = eval_check(eval, obj, 1, &assert_string<size_t>);

        return make_list(utility::split(str_1->to_string(), str_2->to_string()));
    }
};

struct string_substring
{
    inline static const std::string name = "string-substring";

    inline static const std::string doc{ R"(((string-substring STRING FROM TO)

Return a new string that is the subsection [`FROM`, `TO`) of `STRING`.
))" };

    static ALObjectPtr Fstring_substring(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<3>(obj));
        auto str = eval_check(eval, obj, 0, &assert_string<size_t>);

        auto ind_1 = eval_check(eval, obj, 1, &assert_int<size_t>);

        auto ind_2 = eval_check(eval, obj, 2, &assert_int<size_t>);

        return make_string(str->to_string().substr(static_cast<ALObject::string_type::size_type>(ind_1->to_int()),
                                                   static_cast<ALObject::string_type::size_type>(ind_2->to_int())));
    }
};

struct string_splitlines
{
    inline static const std::string name = "string-splitlines";

    inline static const std::string doc{ R"((string-splitlines STRING)

Split `STRING` based on `\n` and return a list of the lines.
)" };

    static ALObjectPtr Fstring_splitlines(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        return make_list(utility::split(str_1->to_string(), '\n'));
    }
};

struct string_upper
{
    inline static const std::string name = "string-upper";

    inline static const std::string doc{ R"((string-upper STRING)

Capitalize every letter of `STRING`.
)" };

    static ALObjectPtr Fstring_upper(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        return make_string(utility::str_toupper(str_1->to_string()));
    }
};

struct string_lower
{
    inline static const std::string name = "string-lower";

    inline static const std::string doc{ R"((string-lower STRING)

Lower every letter of `STRING`.
)" };

    static ALObjectPtr Fstring_lower(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        return make_string(utility::str_tolower(str_1->to_string()));
    }
};

struct string_strip
{
    inline static const std::string name = "string-strip";

    inline static const std::string doc{ R"((string-strip STRING)

Remove and trailing or preceding whitespace of string.
)" };

    static ALObjectPtr Fstring_strip(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        return make_string(utility::trim(str_1->to_string()));
    }
};

struct string_join
{
    inline static const std::string name = "string-join";

    inline static const std::string doc{ R"((string-join STRING [[STRING] ...])

Concatenate the provided string to a new string.
)" };

    static ALObjectPtr Fstring_join(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
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
};

}  // namespace alisp
