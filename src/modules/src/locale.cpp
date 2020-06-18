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


ALObjectPtr Fnum_true_name(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fnum_false_name(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fnum_thousand_sep(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fnum_decimal_point(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_positive_sign(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_negative_sign(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_symobl(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_thousand_sep(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_decimal_point(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fput_money(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_number(obj));

    if (std::size(*t_obj) > 1)
    {
        auto id = eval->eval(t_obj->i(1));
        AL_CHECK(assert_int(id));
        auto s =
          detail::str_with_locale<std::money_put<char>>(detail::loc_registry[object_to_resource(id)], obj->to_real());
        return make_string(s);
    }

    auto s = detail::str_with_locale<std::money_put<char>>(detail::g_defautl_loc, obj->to_real());
    return make_string(s);
}

ALObjectPtr Fput_num(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fput_time(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisspace(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisblank(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fiscntrl(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisupper(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fislower(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisalpha(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisdigit(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fispunct(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisxdigit(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisalnum(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisprint(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisgraph(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Freset_locale(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));

    detail::g_defautl_loc = std::locale();
    return Qt;
}

ALObjectPtr Fset_preffered(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));

    detail::g_defautl_loc = std::locale{ "" };
    return Qt;
}

ALObjectPtr Flocale(ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(obj));

    auto id = detail::loc_registry.emplace_resource(obj->to_string())->id;
    env->defer_callback([id = id]() { detail::loc_registry.destroy_resource(id); });

    return resource_to_object(id);
}

ALObjectPtr Fset_default_locale(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Flocale_name(ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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


}  // namespace loc

ALISP_EXPORT alisp::env::ModulePtr init_locale(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mlocale = alisp::module_init("locale");
    auto loc_ptr = Mlocale.get();

    alisp::module_doc(loc_ptr, R"(The `locale` module provides support for dealing with POSIX
locales. This allows developers to handle culture specific issues in
an application.

Internally the module uses the standard C++ library and proves
"sensible" access to the underlying functions.

Locales can be created through the `locale` function with a name such
as `en_US.utf8` or 'en_GB.utf8'. You can execute `locale -a` in a
terminal to see all of the locales that the host system supports. A
valid locale id is any one of the these locales.

)");

    alisp::module_defun(loc_ptr,
                        "locale",
                        &loc::Flocale,
                        R"((locale ID)

Create a new locale with the given id as s tring and return a resource
object for it.
)");

    alisp::module_defun(loc_ptr,
                        "locale-name",
                        &loc::Flocale_name,
                        R"((locale-name [LOCALE]

Return the name of the global default locale. If `LOCALE` is given,
return its name.
))");

    alisp::module_defun(loc_ptr,
                        "set-locale",
                        &loc::Fset_default_locale,
                        R"((set-locale LOCALE)

Change the current global default locale to `LOCALE`.
)");

    alisp::module_defun(loc_ptr,
                        "set-preffered-locale",
                        &loc::Fset_preffered,
                        R"((set-preffered-locale)

Set the global locale to the preffered (according to the host system) locale.
)");

    alisp::module_defun(loc_ptr,
                        "reset-locale",
                        &loc::Freset_locale,
                        R"((reset-locale)

Reset the global locate to the default locale.
)");


    alisp::module_defun(loc_ptr,
                        "isgraph",
                        &loc::Fisgraph,
                        R"((isgraph CHAR [LOCALE])

Check if `CHAR` is a graphical character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isprint",
                        &loc::Fisprint,
                        R"((isprint CHAR [LOCALE])

Check if `CHAR` is a printable character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isalnum",
                        &loc::Fisalnum,
                        R"((isalnum CHAR [LOCALE])

Check if `CHAR` is alpha-numerical character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isxdigit",
                        &loc::Fisxdigit,
                        R"((isxdigit CHAR [LOCALE])

Check if `CHAR` is a hexdecimal digit character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "ispunct",
                        &loc::Fispunct,
                        R"((ispunct CHAR [LOCALE])

Check if `CHAR` is a punctuation character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isdigit",
                        &loc::Fisdigit,
                        R"((isdigit CHAR [LOCALE])

Check if `CHAR` is a digit character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isalpha",
                        &loc::Fisalpha,
                        R"((isalpha CHAR [LOCALE])

Check if `CHAR` is a graphical character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "islower",
                        &loc::Fislower,
                        R"((islower CHAR [LOCALE])

Check if `CHAR` is a lower case character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isupper",
                        &loc::Fisupper,
                        R"((isupper CHAR [LOCALE])

Check if `CHAR` is a upper case character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "iscntrl",
                        &loc::Fiscntrl,
                        R"((iscntrl CHAR [LOCALE])

Check if `CHAR` is a control character according to the default
locale or to `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "isblank",
                        &loc::Fisblank,
                        R"((isblank CHAR [LOCALE])

Check if `CHAR` is a blank character according to the default
locale or to `LOCALE` if given.)");

    alisp::module_defun(loc_ptr,
                        "isspace",
                        &loc::Fisspace,
                        R"((isspace CHAR [LOCALE])

Check if `CHAR` is a space character according to the default
locale or to `LOCALE` if given.
)");


    alisp::module_defun(loc_ptr,
                        "put-time",
                        &loc::Fput_time,
                        R"((put-time TIME FORMAT-STRING [LOCALE])

Return a fromated string of `FORMAT-STRING` with `TIME` acording to
the global locale or `LOCALE` if given.
)");


    alisp::module_defun(loc_ptr,
                        "put-num",
                        &loc::Fput_num,
                        R"((put-num NUMBER [LOCALE])

Return a textural representation of `NUMBER` as a number according to
the global locale or `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "put-money",
                        &loc::Fput_money,
                        R"((put-money NUMBER [LOCALE])

Return a textural representation of `NUMBER` as currency according to
the global locale or `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "money-decimal-point",
                        &loc::Fmoney_decimal_point,
                        R"((money-decimal-point [LOCALE])

Return the character used for deciaml point in money strings as used
by the global locale or the `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "money-thousand-sep",
                        &loc::Fmoney_thousand_sep,
                        R"((money-thousand-sep [LOCALE])

Return the character used for deciaml point in money strings as used
by the global locale or the `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "money-symobl",
                        &loc::Fmoney_symobl,
                        R"((money-symbol [LOCALE])

Return the character used to represent the local currrency accrding by
the global locale or the `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "money-negative-sign",
                        &loc::Fmoney_negative_sign,
                        R"((money-negative-sign [LOCALE])

Return the character used to represent negative amount of money
accrding by the global locale or the `LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "money-positive-sign",
                        &loc::Fmoney_positive_sign,
                        R"((money-positive-sign [LOCALE])

Return the character used to represent positive amount of money
accrding by the global locale or the `LOCALE` if given.
 )");

    alisp::module_defun(loc_ptr,
                        "num-decimal-point",
                        &loc::Fnum_decimal_point,
                        R"((num-decimal-point [LOCALE])

Return the character used for decimal point in textural representation
of real numbers according to the global locale or the `LOCALE` if
given.
)");

    alisp::module_defun(loc_ptr,
                        "num-thousand-sep",
                        &loc::Fnum_thousand_sep,
                        R"((num-thousand-sep [LOCALE])

Return the character used to separate the thousands in the textual
representation of real numbers according to the global locale or the
`LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "num-false-name",
                        &loc::Fnum_false_name,
                        R"((num-false-name [LOCALE])

Return the string used to represent `false` according to the global locale or the
`LOCALE` if given.
)");

    alisp::module_defun(loc_ptr,
                        "num-true-name",
                        &loc::Fnum_true_name,
                        R"((num-true-name [LOCALE])

Return the string used to represent `true` according to the global locale or the
`LOCALE` if given.
)");


    return Mlocale;
}
