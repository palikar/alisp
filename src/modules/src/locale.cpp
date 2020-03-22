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
    return make_string(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).truename());
}

ALObjectPtr Fnum_false_name(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_string(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).falsename());
}

ALObjectPtr Fnum_thousand_sep(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_char(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).thousands_sep());
}

ALObjectPtr Fnum_decimal_point(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_char(std::use_facet<std::numpunct<char>>(detail::g_defautl_loc).decimal_point());
}

ALObjectPtr Fmoney_positive_sign(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_string(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).positive_sign());
}

ALObjectPtr Fmoney_negative_sign(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_string(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).negative_sign());
}

ALObjectPtr Fmoney_symobl(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_string(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).curr_symbol());
}

ALObjectPtr Fmoney_thousand_sep(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_char(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).thousands_sep());
}

ALObjectPtr Fmoney_decimal_point(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<1>(t_obj));
    return make_char(std::use_facet<std::moneypunct<char>>(detail::g_defautl_loc).decimal_point());
}

ALObjectPtr Fput_money(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_number(obj));

    auto s = detail::str_with_locale<std::money_put<char>>(detail::g_defautl_loc, obj->to_real());
    return make_string(s);
}

ALObjectPtr Fnum_money(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_number(obj));

    std::stringstream ss;
    ss.imbue(std::locale());
    std::use_facet<std::num_put<char>>(std::cout.getloc()).put({ ss }, ss, ' ', obj->to_real());

    return make_string(ss.str());
}

ALObjectPtr Fput_time(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    auto obj_fmt = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(obj_fmt));
    auto fmt = obj_fmt->to_string();


    std::stringstream ss;
    ss.imbue(std::locale());
    std::use_facet<std::time_put<char>>(std::cout.getloc())
      .put({ ss }, ss, ' ', std::localtime(&t), &fmt[0], &fmt[0] + fmt.size());

    return make_string(ss.str());
}

ALObjectPtr Fisspace(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isspace(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisblank(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isblank(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fiscntrl(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::iscntrl(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisupper(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isupper(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fislower(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::islower(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisalpha(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isalpha(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisdigit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isdigit(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fispunct(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::ispunct(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisxdigit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isxdigit(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisalnum(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isalnum(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisprint(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isprint(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Fisgraph(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));

    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(obj));
    auto t = obj->to_int();

    return std::isgraph(t, detail::g_defautl_loc) ? Qt : Qnil;
}

ALObjectPtr Freset_locale(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<0>(t_obj));

    detail::g_defautl_loc = std::locale();
    return Qt;
}

ALObjectPtr Fset_preffered(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<0>(t_obj));

    detail::g_defautl_loc = std::locale{ "" };
    return Qt;
}

ALObjectPtr Flocale(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(obj));

    auto id = detail::loc_registry.emplace_resource(obj->to_string())->id;

    return resource_to_object(id);
}

ALObjectPtr Fset_default_locale(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto obj = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(obj));

    auto id = object_to_resource(obj);

    if (detail::loc_registry.belong(id))
    {
        detail::g_defautl_loc = detail::loc_registry[id];
        return Qt;
    }

    return Qnil;
}


}  // namespace loc

ALISP_EXPORT alisp::env::ModulePtr init_locale(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mlocale = alisp::module_init("locale");
    auto loc_ptr = Mlocale.get();

    alisp::module_doc(loc_ptr, R"()");

    alisp::module_defun(loc_ptr, "put-money", &loc::Fput_money);

    return Mlocale;
}
