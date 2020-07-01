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

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/management/registry.hpp"

#include <locale>

namespace loc
{
using namespace alisp;

auto loc_signal = alisp::make_symbol("locale-signal");


namespace detail
{

static std::locale g_defautl_loc;

inline management::Registry<std::locale, 0x08> loc_registry;

template<typename Facet, typename T> auto str_with_locale(const std::locale &t_loc, T &&t_obj)
{

    std::stringstream ss;
    ss.imbue(t_loc);
    auto &f = std::use_facet<Facet>(t_loc);
    f.put({ ss }, false, ss, ss.fill(), t_obj);

    return ss.str();
}


}  // namespace detail


struct num_true_name
{
    inline static const std::string name{ "num-true-name" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((num-true-name [LOCALE])

Return the string used to represent `true` according to the global locale or the
`LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_string(
              std::use_facet<std::numpunct<char>>(detail::loc_registry[object_to_resource(id)]).truename());
        }

        return make_string(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).truename());
    }
};

struct num_false_name
{
    inline static const std::string name{ "num-false-name" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((num-false-name [LOCALE])

Return the string used to represent `false` according to the global locale or the
`LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_string(
              std::use_facet<std::numpunct<char>>(detail::loc_registry[object_to_resource(id)]).falsename());
        }

        return make_string(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).falsename());
    }
};

struct num_thousand_sep
{
    inline static const std::string name{ "num-thousand-sep" };

    inline static const Signature signature{ Optional{}, Int{} };

    inline static const std::string doc{ R"((num-thousand-sep [LOCALE])

Return the character used to separate the thousands in the textual
representation of real numbers according to the global locale or the
`LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_char(
              std::use_facet<std::numpunct<char>>(detail::loc_registry[object_to_resource(id)]).thousands_sep());
        }

        return make_char(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).thousands_sep());
    }
};

struct num_decimal_point
{
    inline static const std::string name{ "num-decimal-point" };

    inline static const Signature signature{ Optional{}, Int{} };

    inline static const std::string doc{ R"((num-decimal-point [LOCALE])

Return the character used for decimal point in textural representation
of real numbers according to the global locale or the `LOCALE` if
given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_char(
              std::use_facet<std::numpunct<char>>(detail::loc_registry[object_to_resource(id)]).decimal_point());
        }

        return make_char(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).decimal_point());
    }
};

struct money_positive_sign
{
    inline static const std::string name{ "money-positive-sign" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((money-positive-sign [LOCALE])

Return the character used to represent positive amount of money
accrding by the global locale or the `LOCALE` if given.
 )" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_string(
              std::use_facet<std::moneypunct<char>>(detail::loc_registry[object_to_resource(id)]).positive_sign());
        }

        return make_string(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).positive_sign());
    }
};

struct money_negative_sign
{
    inline static const std::string name{ "money-negative-sign" };

    inline static const Signature signature{ Optional{}, Int{} };

    inline static const std::string doc{ R"((money-negative-sign [LOCALE])

Return the character used to represent negative amount of money
accrding by the global locale or the `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_string(
              std::use_facet<std::moneypunct<char>>(detail::loc_registry[object_to_resource(id)]).negative_sign());
        }

        return make_string(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).negative_sign());
    }
};

struct money_symobl
{
    inline static const std::string name{ "money-symobl" };

    inline static const Signature signature{ Optional{}, Int{} };

    inline static const std::string doc{ R"((money-symbol [LOCALE])

Return the character used to represent the local currrency accrding by
the global locale or the `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_string(
              std::use_facet<std::moneypunct<char>>(detail::loc_registry[object_to_resource(id)]).curr_symbol());
        }

        return make_string(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).curr_symbol());
    }
};

struct money_thousand_sep
{
    inline static const std::string name{ "money-thousand-sep" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((money-thousand-sep [LOCALE])

Return the character used for deciaml point in money strings as used
by the global locale or the `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_char(
              std::use_facet<std::moneypunct<char>>(detail::loc_registry[object_to_resource(id)]).thousands_sep());
        }

        return make_char(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).thousands_sep());
    }
};

struct money_decimal_point
{
    inline static const std::string name{ "money-decimal-point" };

    inline static const Signature signature{ Optional{}, Int{} };

    inline static const std::string doc{ R"((money-decimal-point [LOCALE])

Return the character used for deciaml point in money strings as used
by the global locale or the `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto id = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(id));
            return make_char(
              std::use_facet<std::moneypunct<char>>(detail::loc_registry[object_to_resource(id)]).decimal_point());
        }

        return make_char(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).decimal_point());
    }
};

struct put_money
{
    inline static const std::string name{ "put-money" };

    inline static const Signature signature{ Number{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((put-money NUMBER [LOCALE])

Return a textural representation of `NUMBER` as currency according to
the global locale or `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));
        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_number(obj));

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            auto s = detail::str_with_locale<std::money_put<char>>(detail::loc_registry[object_to_resource(id)],
                                                                   obj->to_real());
            return make_string(s);
        }

        auto s = detail::str_with_locale<std::money_put<char>>(detail::g_defautl_loc, obj->to_real());
        return make_string(s);
    }
};

struct put_num
{
    inline static const std::string name{ "put-num" };

    inline static const Signature signature{ Number{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((put-num NUMBER [LOCALE])

Return a textural representation of `NUMBER` as a number according to
the global locale or `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));
        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_number(obj));

        std::stringstream ss;

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            ss.imbue(detail::loc_registry[object_to_resource(id)]);
        }
        else
        {
            ss.imbue(std::locale());
        }

        std::use_facet<std::num_put<char>>(std::cout.getloc()).put({ ss }, ss, ' ', obj->to_real());

        return make_string(ss.str());
    }
};

struct put_time
{
    inline static const std::string name{ "put-time" };

    inline static const Signature signature{ Int{}, String{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((put-time TIME FORMAT-STRING [LOCALE])

Return a fromated string of `FORMAT-STRING` with `TIME` acording to
the global locale or `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<2>(t_obj));
        AL_CHECK(assert_max_size<3>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        auto obj_fmt = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(obj_fmt));
        auto fmt = obj_fmt->to_string();

        std::stringstream ss;

        if (std::size(*t_obj) > 2)
        {
            auto id = eval->eval(t_obj->i(2));
            AL_CHECK(assert_int(id));
            ss.imbue(detail::loc_registry[object_to_resource(id)]);
        }
        else
        {
            ss.imbue(std::locale());
        }


        ss.imbue(std::locale());
        std::use_facet<std::time_put<char>>(ss.getloc())
          .put({ ss }, ss, ' ', std::localtime(&t), &fmt[0], &fmt[0] + fmt.size());

        return make_string(ss.str());
    }
};

struct isspace
{
    inline static const std::string name{ "isspace" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isspace CHAR [LOCALE])

Check if `CHAR` is a space character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isspace(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }


        return std::isspace(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isblank
{
    inline static const std::string name{ "isblank" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isblank CHAR [LOCALE])

Check if `CHAR` is a blank character according to the default
locale or to `LOCALE` if given.)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isblank(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isblank(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct iscntrl
{
    inline static const std::string name{ "iscntrl" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((iscntrl CHAR [LOCALE])

Check if `CHAR` is a control character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::iscntrl(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::iscntrl(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isupper
{
    inline static const std::string name{ "isupper" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isupper CHAR [LOCALE])

Check if `CHAR` is a upper case character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isupper(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isupper(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct islower
{
    inline static const std::string name{ "islower" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((islower CHAR [LOCALE])

Check if `CHAR` is a lower case character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::islower(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::islower(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isalpha
{
    inline static const std::string name{ "isalpha" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isalpha CHAR [LOCALE])

Check if `CHAR` is a graphical character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isalpha(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isalpha(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isdigit
{
    inline static const std::string name{ "isdigit" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isdigit CHAR [LOCALE])

Check if `CHAR` is a digit character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isdigit(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isdigit(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct ispunct
{
    inline static const std::string name{ "ispunct" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((ispunct CHAR [LOCALE])

Check if `CHAR` is a punctuation character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::ispunct(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::ispunct(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isxdigit
{
    inline static const std::string name{ "isxdigit" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isxdigit CHAR [LOCALE])

Check if `CHAR` is a hexdecimal digit character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isxdigit(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isxdigit(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isalnum
{
    inline static const std::string name{ "isalnum" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isalnum CHAR [LOCALE])

Check if `CHAR` is alpha-numerical character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isalnum(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isalnum(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isprint
{
    inline static const std::string name{ "isprint" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isprint CHAR [LOCALE])

Check if `CHAR` is a printable character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isprint(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isprint(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct isgraph
{
    inline static const std::string name{ "isgraph" };

    inline static const Signature signature{ Char{}, Optional{}, Int{} };

    inline static const std::string doc{ R"((isgraph CHAR [LOCALE])

Check if `CHAR` is a graphical character according to the default
locale or to `LOCALE` if given.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<2>(t_obj));

        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_char(obj));
        auto t = obj->to_int();

        if (std::size(*t_obj) > 1)
        {
            auto id = eval->eval(t_obj->i(1));
            AL_CHECK(assert_int(id));
            return std::isgraph(char(t), detail::loc_registry[object_to_resource(id)]) ? Qt : Qnil;
        }

        return std::isgraph(char(t), detail::g_defautl_loc) ? Qt : Qnil;
    }
};

struct reset_locale
{
    inline static const std::string name{ "reset-locale" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"((reset-locale)

Reset the global locate to the default locale.
)" };


    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<0>(t_obj));

        detail::g_defautl_loc = std::locale();
        return Qt;
    }
};

struct set_preffered
{
    inline static const std::string name{ "set-preffered-locale" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((set-preffered-locale)

Set the global locale to the preffered (according to the host system) locale.
)" };


    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<0>(t_obj));

        detail::g_defautl_loc = std::locale{ "" };
        return Qt;
    }
};

struct locale
{
    inline static const std::string name{ "locale" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"((locale ID)

Create a new locale with the given id as s tring and return a resource
object for it.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(obj));

        try
        {
            auto id = detail::loc_registry.emplace_resource(obj->to_string())->id;
            env->defer_callback([id = id]() { detail::loc_registry.destroy_resource(id); });
            return resource_to_object(id);
        }
        catch (std::runtime_error &exp)
        {
            signal(loc_signal,
                   std::string("LOCALE ERROR: Cannot consturct locale '") + obj->to_string() + "'; " + exp.what()
                     + "\n");
            return Qnil;
        }
    }
};

struct set_default_locale
{
    inline static const std::string name{ "set-locale" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"((set-locale LOCALE)

Change the current global default locale to `LOCALE`.
)" };


    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto obj = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(obj));

        auto id = object_to_resource(obj);

        if (detail::loc_registry.belong(id))
        {
            detail::g_defautl_loc = detail::loc_registry[id];
            return Qt;
        }

        return Qnil;
    }
};

struct locale_name
{
    inline static const std::string name{ "locale-name" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((locale-name [LOCALE]

Return the name of the global default locale. If `LOCALE` is given,
return its name.
))" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_max_size<1>(t_obj));

        if (std::size(*t_obj) > 0)
        {
            auto obj = eval->eval(t_obj->i(0));
            AL_CHECK(assert_int(obj));
            return make_string(detail::loc_registry[object_to_resource(obj)].name());
        }

        return make_string(detail::g_defautl_loc.name());
    }
};

struct module_doc
{

    inline static const std::string doc{ R"((The `locale` module provides support for dealing with POSIX
locales. This allows developers to handle culture specific issues in
an application.

Internally the module uses the standard C++ library and proves
"sensible" access to the underlying functions.

Locales can be created through the `locale` function with a name such
as `en_US.utf8` or 'en_GB.utf8'. You can execute `locale -a` in a
terminal to see all of the locales that the host system supports. A
valid locale id is any one of the these locales.

))" };
};


}  // namespace loc

ALISP_EXPORT alisp::env::ModulePtr init_locale(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mlocale = alisp::module_init("locale");
    auto loc_ptr = Mlocale.get();

    module_doc(loc_ptr, loc::module_doc::doc);

    module_defun(
      loc_ptr, loc::locale_name::name, loc::locale_name::func, loc::locale_name::doc, loc::locale_name::signature.al());
    module_defun(loc_ptr,
                 loc::set_default_locale::name,
                 loc::set_default_locale::func,
                 loc::set_default_locale::doc,
                 loc::set_default_locale::signature.al());
    module_defun(loc_ptr, loc::locale::name, loc::locale::func, loc::locale::doc, loc::locale::signature.al());
    module_defun(loc_ptr,
                 loc::num_true_name::name,
                 loc::num_true_name::func,
                 loc::num_true_name::doc,
                 loc::num_true_name::signature.al());
    module_defun(loc_ptr,
                 loc::num_thousand_sep::name,
                 loc::num_thousand_sep::func,
                 loc::num_thousand_sep::doc,
                 loc::num_thousand_sep::signature.al());
    module_defun(loc_ptr,
                 loc::num_decimal_point::name,
                 loc::num_decimal_point::func,
                 loc::num_decimal_point::doc,
                 loc::num_decimal_point::signature.al());
    module_defun(loc_ptr,
                 loc::money_positive_sign::name,
                 loc::money_positive_sign::func,
                 loc::money_positive_sign::doc,
                 loc::money_positive_sign::signature.al());
    module_defun(loc_ptr,
                 loc::money_negative_sign::name,
                 loc::money_negative_sign::func,
                 loc::money_negative_sign::doc,
                 loc::money_negative_sign::signature.al());
    module_defun(loc_ptr,
                 loc::money_symobl::name,
                 loc::money_symobl::func,
                 loc::money_symobl::doc,
                 loc::money_symobl::signature.al());
    module_defun(loc_ptr,
                 loc::money_thousand_sep::name,
                 loc::money_thousand_sep::func,
                 loc::money_thousand_sep::doc,
                 loc::money_thousand_sep::signature.al());
    module_defun(loc_ptr,
                 loc::money_decimal_point::name,
                 loc::money_decimal_point::func,
                 loc::money_decimal_point::doc,
                 loc::money_decimal_point::signature.al());
    module_defun(
      loc_ptr, loc::put_money::name, loc::put_money::func, loc::put_money::doc, loc::put_money::signature.al());
    module_defun(loc_ptr, loc::put_num::name, loc::put_num::func, loc::put_num::doc, loc::put_num::signature.al());
    module_defun(loc_ptr, loc::put_time::name, loc::put_time::func, loc::put_time::doc, loc::put_time::signature.al());
    module_defun(loc_ptr, loc::isspace::name, loc::isspace::func, loc::isspace::doc, loc::isspace::signature.al());
    module_defun(loc_ptr, loc::isblank::name, loc::isblank::func, loc::isblank::doc, loc::isblank::signature.al());
    module_defun(loc_ptr, loc::iscntrl::name, loc::iscntrl::func, loc::iscntrl::doc, loc::iscntrl::signature.al());
    module_defun(loc_ptr, loc::isupper::name, loc::isupper::func, loc::isupper::doc, loc::isupper::signature.al());
    module_defun(loc_ptr, loc::islower::name, loc::islower::func, loc::islower::doc, loc::islower::signature.al());
    module_defun(loc_ptr, loc::isalpha::name, loc::isalpha::func, loc::isalpha::doc, loc::isalpha::signature.al());
    module_defun(loc_ptr, loc::isdigit::name, loc::isdigit::func, loc::isdigit::doc, loc::isdigit::signature.al());
    module_defun(loc_ptr, loc::ispunct::name, loc::ispunct::func, loc::ispunct::doc, loc::ispunct::signature.al());
    module_defun(loc_ptr, loc::isxdigit::name, loc::isxdigit::func, loc::isxdigit::doc, loc::isxdigit::signature.al());
    module_defun(loc_ptr, loc::isalnum::name, loc::isalnum::func, loc::isalnum::doc, loc::isalnum::signature.al());
    module_defun(loc_ptr, loc::isprint::name, loc::isprint::func, loc::isprint::doc, loc::isprint::signature.al());
    module_defun(loc_ptr, loc::isgraph::name, loc::isgraph::func, loc::isgraph::doc, loc::isgraph::signature.al());
    module_defun(loc_ptr,
                 loc::reset_locale::name,
                 loc::reset_locale::func,
                 loc::reset_locale::doc,
                 loc::reset_locale::signature.al());
    module_defun(loc_ptr,
                 loc::set_preffered::name,
                 loc::set_preffered::func,
                 loc::set_preffered::doc,
                 loc::set_preffered::signature.al());
    module_defun(loc_ptr, loc::locale::name, loc::locale::func, loc::locale::doc, loc::locale::signature.al());


    return Mlocale;
}
