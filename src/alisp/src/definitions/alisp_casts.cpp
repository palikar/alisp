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

#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_signature.hpp"

#include "alisp/alisp/declarations/parse.hpp"

namespace alisp
{


struct Sint_parse
{
    inline static const std::string name = "parse-int";

    inline static const std::string doc{ R"((parse-int STRING)

Return the int value represented by STRING.

Example:
```elisp
(parse-int "12")
```
)" };

    static ALObjectPtr Fint_parse(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        try
        {
            return make_int(std::stoi(str_1->to_string()));
        }
        catch (std::invalid_argument &ex)
        {
            throw eval_error("The argument is not a valid integer value.");
        }
        catch (std::out_of_range &ex)
        {
            throw eval_error("The given integer value is too big.");
        }
        return Qnil;
    }
};

struct Sfloat_parse
{
    inline static const std::string name = "parse-float";

    inline static const std::string doc{ R"((parse-float STRING)

Return the real value represented by STRING.

Example:
```elisp
(parse-float "12.32")
```
)" };

    static ALObjectPtr Ffloat_parse(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto str_1 = eval_check(eval, obj, 0, &assert_string<size_t>);

        try
        {
            return make_double(std::stod(str_1->to_string()));
        }
        catch (std::invalid_argument &ex)
        {
            throw eval_error("The argument is not a valid float value.");
        }
        catch (std::out_of_range &ex)
        {
            throw eval_error("The given float value is too big.");
        }
        return Qnil;
    }
};

struct Sto_string
{
    inline static const std::string name = "to-string";

    inline static const std::string doc{ R"((to-string VALUE)

Convert VALUE to string

Example:
```elisp
(to-string 42)
(to-string 42.32)
(to-string "string")
```
)" };

    static ALObjectPtr Fto_string(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));

        return make_visit(
          eval->eval(t_obj->i(0)),
          is_function() >>= [](ALObjectPtr obj) { return make_string(obj->get_prop("--name--")->to_string()); },
          is_char() >>= [](ALObjectPtr obj) { return make_string(std::string(1, char(obj->to_int()))); },
          type(ALObjectType::INT_VALUE) >>= [](ALObjectPtr obj) { return make_string(std::to_string(obj->to_int())); },
          type(ALObjectType::REAL_VALUE) >>=
          [](ALObjectPtr obj) {
              std::stringstream ss;
              ss << obj->to_real();
              return make_string(ss.str());
          },
          type(ALObjectType::STRING_VALUE) >>= [](ALObjectPtr obj) { return make_string(obj->to_string()); },
          type(ALObjectType::SYMBOL) >>= [](ALObjectPtr obj) { return make_string(obj->to_string()); },
          any_pattern() >>= [](ALObjectPtr) { return Qnil; }

        );
    }
};

struct Sto_char
{
    inline static const std::string name{ "to-char" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"((to-char INT)

Convert INT to a character (ASCII encoding). INT must be a value in
the range [0, 255].

Example:
```elisp
(to-char 65)
(to-char 97)
```
)" };


    static ALObjectPtr Fto_char(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));

        auto ch = eval->eval(t_obj->i(0));

        if (pstring(ch))
        {
            auto value = ch->to_string();
            if (value.size() != 1)
            {
                return Qnil;
            }
            return make_char(value[0]);
        }
        else if (pint(ch))
        {
            auto value = ch->to_int();
            if (!(0 <= value && value <= 127))
            {
                return Qnil;
            }
            return make_char(value);
        }
        else
        {
            return Qnil;
        }
    }
};


}  // namespace alisp
