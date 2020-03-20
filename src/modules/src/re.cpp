#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/management/registry.hpp"

#include <regex>

namespace re
{
using namespace alisp;

ALObjectPtr reflag_default = make_symbol("re-default");
ALObjectPtr reflag_not_bol = make_symbol("re-not-bol");
ALObjectPtr reflag_not_eol = make_symbol("re-not-eol");
ALObjectPtr reflag_not_bow = make_symbol("re-not-bow");
ALObjectPtr reflag_not_eow = make_symbol("re-not-eow");
ALObjectPtr reflag_any = make_symbol("re-any");
ALObjectPtr reflag_not_null = make_symbol("re-not-null");
ALObjectPtr reflag_continous = make_symbol("re-continous");
ALObjectPtr reflag_prev_avail = make_symbol("re-prev-avail");
ALObjectPtr reflag_format_default = make_symbol("re-format-default");
ALObjectPtr reflag_format_sed = make_symbol("re-format-sed");
ALObjectPtr reflag_format_no_copy = make_symbol("re-format-no-copy");
ALObjectPtr reflag_format_first_only = make_symbol("re-format-no-first-only");

ALObjectPtr reflag_icase = make_symbol("re-icase");
ALObjectPtr reflag_nosubs = make_symbol("re-nosubs");
ALObjectPtr reflag_optimize = make_symbol("re-optimize");
ALObjectPtr reflag_collate = make_symbol("re-collate");
ALObjectPtr reflag_ecma_script = make_symbol("re-ecma");
ALObjectPtr reflag_basic = make_symbol("re-basic");
ALObjectPtr reflag_extended = make_symbol("re-extended");
ALObjectPtr reflag_awk = make_symbol("re-awk");
ALObjectPtr reflag_grep = make_symbol("re-grep");
ALObjectPtr reflag_egrep = make_symbol("re-egrep");

namespace detail
{

inline management::Registry<std::regex, 0x07> reg_registry;

ALObjectPtr match_to_obj(std::smatch& t_match)
{
    ALObject::list_type matches{};
    matches.reserve(t_match.size());

    for (auto& el : t_match) {
        matches.push_back(make_string(el.str()));
    }
    return make_list(matches);
}

std::regex obj_to_regex(ALObjectPtr t_obj)
{
    if (pstring(t_obj)) {
        return std::regex(t_obj->to_string());
    } else if (pint(t_obj)) {
        auto i = object_to_resource(t_obj);
        if (reg_registry.belong(i)) {
            return reg_registry[i];
        }
    }
    return {};
}

std::regex_constants::match_flag_type handle_match_flags(ALObjectPtr t_obj)
{
    std::regex_constants::match_flag_type val = std::regex_constants::match_default;

    for (auto& el : *t_obj) {

        if (eq(el, reflag_default)) {
            val |= std::regex_constants::match_default;
        }

        if (eq(el, reflag_not_bol)) {
            val |= std::regex_constants::match_not_bol;
        }

        if (eq(el, reflag_not_eol)) {
            val |= std::regex_constants::match_not_eol;
        }

        if (eq(el, reflag_not_bow)) {
            val |= std::regex_constants::match_not_bow;
        }
        if (eq(el, reflag_not_eow)) {
            val |= std::regex_constants::match_not_eow;
        }

        if (eq(el, reflag_any)) {
            val |= std::regex_constants::match_any;
        }

        if (eq(el, reflag_not_null)) {
            val |= std::regex_constants::match_not_null;
        }

        if (eq(el, reflag_continous)) {
            val |= std::regex_constants::match_continuous;
        }

        if (eq(el, reflag_prev_avail)) {
            val |= std::regex_constants::match_prev_avail;
        }

        if (eq(el, reflag_format_default)) {
            val |= std::regex_constants::format_default;
        }

        if (eq(el, reflag_format_sed)) {
            val |= std::regex_constants::format_sed;
        }

        if (eq(el, reflag_format_no_copy)) {
            val |= std::regex_constants::format_no_copy;
        }

        if (eq(el, reflag_format_first_only)) {
            val |= std::regex_constants::format_first_only;
        }
    }

    return val;
}

std::regex_constants::syntax_option_type handle_regex_flags(ALObjectPtr t_obj)
{
    std::regex_constants::syntax_option_type val = std::regex_constants::ECMAScript;

    for (auto& el : *t_obj) {
        if (eq(el, reflag_icase)) {
            val |= std::regex_constants::icase;
        }

        if (eq(el, reflag_nosubs)) {
            val |= std::regex_constants::nosubs;
        }

        if (eq(el, reflag_optimize)) {
            val |= std::regex_constants::optimize;
        }

        if (eq(el, reflag_collate)) {
            val |= std::regex_constants::collate;
            }

        if (eq(el, reflag_ecma_script)) {
            val |= std::regex_constants::ECMAScript;
        }

        if (eq(el, reflag_basic)) {
            val |= std::regex_constants::basic;
        }

        if (eq(el, reflag_extended)) {
            val |= std::regex_constants::extended;
        }

        if (eq(el, reflag_awk)) {
            val |= std::regex_constants::awk;
        }

        if (eq(el, reflag_grep)) {
            val |= std::regex_constants::grep;
        }

        if (eq(el, reflag_egrep)) {
            val |= std::regex_constants::egrep;
        }

    }

    return val;
}

}

ALObjectPtr Freplace(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));

    auto repl = eval->eval(t_obj->i(2));
    AL_CHECK(assert_string(repl));

    std::regex_constants::match_flag_type flags = std::regex_constants::match_default;
    if (std::size(*t_obj) > 1) {
        auto flags_list = eval->eval(t_obj->i(1));
        AL_CHECK(assert_list(flags_list));
        flags = detail::handle_match_flags(flags_list);
    }


    auto res = std::regex_replace(str->to_string(), detail::obj_to_regex(reg), repl->to_string(), flags);

    return make_string(res);
}

ALObjectPtr Fmatch(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));

    std::regex_constants::match_flag_type flags = std::regex_constants::match_default;
    if (std::size(*t_obj) > 1) {
        auto flags_list = eval->eval(t_obj->i(1));
        AL_CHECK(assert_list(flags_list));
        flags = detail::handle_match_flags(flags_list);
    }

    std::smatch match;
    auto res = std::regex_match(str->to_string(), match, detail::obj_to_regex(reg), flags);

    return res ? detail::match_to_obj(match) : Qnil;
}

ALObjectPtr Fsearch(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));

    std::regex_constants::match_flag_type flags = std::regex_constants::match_default;
    if (std::size(*t_obj) > 1) {
        auto flags_list = eval->eval(t_obj->i(1));
        AL_CHECK(assert_list(flags_list));
        flags = detail::handle_match_flags(flags_list);
    }

    std::smatch match;
    auto res = std::regex_search(str->to_string(), match, detail::obj_to_regex(reg), flags);

    return res ? detail::match_to_obj(match) : Qnil;
}

ALObjectPtr Fcompile(ALObjectPtr t_obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));
    auto reg = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(reg));

    std::regex_constants::syntax_option_type flags = std::regex_constants::ECMAScript;
    if (std::size(*t_obj) > 1) {
        auto flags_list = eval->eval(t_obj->i(1));
        AL_CHECK(assert_list(flags_list));
        flags = detail::handle_regex_flags(flags_list);
    }

    auto id = detail::reg_registry.emplace_resource(reg->to_string(), flags)->id;
    env->defer_callback([id = id]() { detail::reg_registry.destroy_resource(id); });
    return resource_to_object(id);
}

}

ALISP_EXPORT alisp::env::ModulePtr init_re(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mre = alisp::module_init("re");
    auto re_ptr = Mre.get();

    alisp::module_doc(re_ptr, R"()");

    alisp::module_defconst(re_ptr, "re-match-default", re::reflag_default);
    alisp::module_defconst(re_ptr, "re-match-not-bol", re::reflag_not_bol);
    alisp::module_defconst(re_ptr, "re-match-not-eol", re::reflag_not_eol);
    alisp::module_defconst(re_ptr, "re-match-not-bow", re::reflag_not_bow);
    alisp::module_defconst(re_ptr, "re-match-not-eow", re::reflag_not_eow);
    alisp::module_defconst(re_ptr, "re-match-any", re::reflag_any);
    alisp::module_defconst(re_ptr, "re-match-not-null", re::reflag_not_null);
    alisp::module_defconst(re_ptr, "re-match-continous", re::reflag_continous);
    alisp::module_defconst(re_ptr, "re-match-prev-avail", re::reflag_prev_avail);
    alisp::module_defconst(re_ptr, "re-match-format-default", re::reflag_format_default);
    alisp::module_defconst(re_ptr, "re-match-format-sed", re::reflag_format_sed);
    alisp::module_defconst(re_ptr, "re-match-format-no-copy", re::reflag_format_no_copy);
    alisp::module_defconst(re_ptr, "re-match-format-first-only", re::reflag_format_first_only);    

    alisp::module_defconst(re_ptr, "re-regex-icase", re::reflag_icase);
    alisp::module_defconst(re_ptr, "re-regex-nosubs", re::reflag_nosubs);
    alisp::module_defconst(re_ptr, "re-regex-optimize", re::reflag_optimize);
    alisp::module_defconst(re_ptr, "re-regex-collate", re::reflag_collate);
    alisp::module_defconst(re_ptr, "re-regex-ecma_script", re::reflag_ecma_script);
    alisp::module_defconst(re_ptr, "re-regex-basic", re::reflag_basic);
    alisp::module_defconst(re_ptr, "re-regex-extended", re::reflag_extended);
    alisp::module_defconst(re_ptr, "re-regex-awk", re::reflag_awk);
    alisp::module_defconst(re_ptr, "re-regex-grep", re::reflag_grep);
    alisp::module_defconst(re_ptr, "re-regex-egrep", re::reflag_egrep);

    alisp::module_defun(re_ptr, "re-compile", &re::Fcompile);

    alisp::module_defun(re_ptr, "re-match", &re::Fmatch);
    alisp::module_defun(re_ptr, "re-search", &re::Fsearch);
    alisp::module_defun(re_ptr, "re-replace", &re::Freplace);



    return Mre;
}
