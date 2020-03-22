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


ALObjectPtr Fnum_true_name(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fnum_false_name(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fnum_thousand_sep(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fnum_decimal_point(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_positive_sign(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_negative_sign(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_symobl(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_thousand_sep(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fmoney_decimal_point(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fput_money(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fput_num(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fput_time(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisspace(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisblank(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fiscntrl(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisupper(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fislower(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisalpha(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisdigit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fispunct(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisxdigit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisalnum(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisprint(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Fisgraph(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Freset_locale(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));

    detail::g_defautl_loc = std::locale();
    return Qt;
}

ALObjectPtr Fset_preffered(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));

    detail::g_defautl_loc = std::locale{ "" };
    return Qt;
}

ALObjectPtr Flocale(ALObjectPtr t_obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(obj));

    auto id = detail::loc_registry.emplace_resource(obj->to_string())->id;
    env->defer_callback([id = id]() { detail::loc_registry.destroy_resource(id); });

    return resource_to_object(id);
}

ALObjectPtr Fset_default_locale(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
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

ALObjectPtr Flocale_name(ALObjectPtr t_obj, env::Environment *env, eval::Evaluator *eval)
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

    alisp::module_doc(loc_ptr, R"()");

    alisp::module_defun(loc_ptr, "locale", &loc::Flocale);
    alisp::module_defun(loc_ptr, "locale-name", &loc::Flocale_name);
    alisp::module_defun(loc_ptr, "set-locale", &loc::Fset_default_locale);
    alisp::module_defun(loc_ptr, "set-preffered-locale", &loc::Fset_preffered);
    alisp::module_defun(loc_ptr, "reset-locale", &loc::Freset_locale);

    alisp::module_defun(loc_ptr, "isgraph", &loc::Fisgraph);
    alisp::module_defun(loc_ptr, "isprint", &loc::Fisprint);
    alisp::module_defun(loc_ptr, "isalnum", &loc::Fisalnum);
    alisp::module_defun(loc_ptr, "isxdigit", &loc::Fisxdigit);
    alisp::module_defun(loc_ptr, "ispunct", &loc::Fispunct);
    alisp::module_defun(loc_ptr, "isdigit", &loc::Fisdigit);
    alisp::module_defun(loc_ptr, "isalpha", &loc::Fisalpha);
    alisp::module_defun(loc_ptr, "islower", &loc::Fislower);
    alisp::module_defun(loc_ptr, "isupper", &loc::Fisupper);
    alisp::module_defun(loc_ptr, "iscntrl", &loc::Fiscntrl);
    alisp::module_defun(loc_ptr, "isblank", &loc::Fisblank);
    alisp::module_defun(loc_ptr, "isspace", &loc::Fisspace);

    alisp::module_defun(loc_ptr, "put-time", &loc::Fput_time);

    alisp::module_defun(loc_ptr, "put-money", &loc::Fput_money);
    alisp::module_defun(loc_ptr, "money-decimal-point", &loc::Fmoney_decimal_point);
    alisp::module_defun(loc_ptr, "money-thousand-sep", &loc::Fmoney_thousand_sep);
    alisp::module_defun(loc_ptr, "money-symobl", &loc::Fmoney_symobl);
    alisp::module_defun(loc_ptr, "money-negative-sign", &loc::Fmoney_negative_sign);
    alisp::module_defun(loc_ptr, "money-positive-sign", &loc::Fmoney_positive_sign);
    alisp::module_defun(loc_ptr, "num-decimal-point", &loc::Fnum_decimal_point);

    alisp::module_defun(loc_ptr, "put-num", &loc::Fput_num);
    alisp::module_defun(loc_ptr, "num-thousand-sep", &loc::Fnum_thousand_sep);
    alisp::module_defun(loc_ptr, "num-false-name", &loc::Fnum_false_name);
    alisp::module_defun(loc_ptr, "num-true-name", &loc::Fnum_true_name);

    return Mlocale;
}
