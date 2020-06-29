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

#include <regex>

namespace re
{
using namespace alisp;

ALObjectPtr reflag_default           = make_symbol("re-default");
ALObjectPtr reflag_not_bol           = make_symbol("re-not-bol");
ALObjectPtr reflag_not_eol           = make_symbol("re-not-eol");
ALObjectPtr reflag_not_bow           = make_symbol("re-not-bow");
ALObjectPtr reflag_not_eow           = make_symbol("re-not-eow");
ALObjectPtr reflag_any               = make_symbol("re-any");
ALObjectPtr reflag_not_null          = make_symbol("re-not-null");
ALObjectPtr reflag_continous         = make_symbol("re-continous");
ALObjectPtr reflag_prev_avail        = make_symbol("re-prev-avail");
ALObjectPtr reflag_format_default    = make_symbol("re-format-default");
ALObjectPtr reflag_format_sed        = make_symbol("re-format-sed");
ALObjectPtr reflag_format_no_copy    = make_symbol("re-format-no-copy");
ALObjectPtr reflag_format_first_only = make_symbol("re-format-no-first-only");

ALObjectPtr reflag_icase       = make_symbol("re-icase");
ALObjectPtr reflag_nosubs      = make_symbol("re-nosubs");
ALObjectPtr reflag_optimize    = make_symbol("re-optimize");
ALObjectPtr reflag_collate     = make_symbol("re-collate");
ALObjectPtr reflag_ecma_script = make_symbol("re-ecma");
ALObjectPtr reflag_basic       = make_symbol("re-basic");
ALObjectPtr reflag_extended    = make_symbol("re-extended");
ALObjectPtr reflag_awk         = make_symbol("re-awk");
ALObjectPtr reflag_grep        = make_symbol("re-grep");
ALObjectPtr reflag_egrep       = make_symbol("re-egrep");

ALObjectPtr re_signal = make_symbol("re-signal");

namespace detail
{

inline management::Registry<std::regex, 0x07> reg_registry;

ALObjectPtr match_to_obj(std::smatch &t_match)
{
    ALObject::list_type matches{};
    matches.reserve(t_match.size());

    for (auto &el : t_match)
    {
        matches.push_back(make_string(el.str()));
    }
    return make_list(matches);
}

std::regex obj_to_regex(ALObjectPtr t_obj)
{
    if (pstring(t_obj))
    {
        return std::regex(t_obj->to_string());
    }
    else if (pint(t_obj))
    {
        auto i = object_to_resource(t_obj);
        if (reg_registry.belong(i))
        {
            return reg_registry[i];
        }
    }
    return {};
}

std::regex_constants::match_flag_type handle_match_flags(ALObjectPtr t_obj)
{
    std::regex_constants::match_flag_type val = std::regex_constants::match_default;

    for (auto &el : *t_obj)
    {

        if (eq(el, reflag_default))
        {
            val |= std::regex_constants::match_default;
        }

        if (eq(el, reflag_not_bol))
        {
            val |= std::regex_constants::match_not_bol;
        }

        if (eq(el, reflag_not_eol))
        {
            val |= std::regex_constants::match_not_eol;
        }

        if (eq(el, reflag_not_bow))
        {
            val |= std::regex_constants::match_not_bow;
        }
        if (eq(el, reflag_not_eow))
        {
            val |= std::regex_constants::match_not_eow;
        }

        if (eq(el, reflag_any))
        {
            val |= std::regex_constants::match_any;
        }

        if (eq(el, reflag_not_null))
        {
            val |= std::regex_constants::match_not_null;
        }

        if (eq(el, reflag_continous))
        {
            val |= std::regex_constants::match_continuous;
        }

        if (eq(el, reflag_prev_avail))
        {
            val |= std::regex_constants::match_prev_avail;
        }

        if (eq(el, reflag_format_default))
        {
            val |= std::regex_constants::format_default;
        }

        if (eq(el, reflag_format_sed))
        {
            val |= std::regex_constants::format_sed;
        }

        if (eq(el, reflag_format_no_copy))
        {
            val |= std::regex_constants::format_no_copy;
        }

        if (eq(el, reflag_format_first_only))
        {
            val |= std::regex_constants::format_first_only;
        }
    }

    return val;
}

std::regex_constants::syntax_option_type handle_regex_flags(ALObjectPtr t_obj)
{
    std::regex_constants::syntax_option_type val = std::regex_constants::ECMAScript;

    for (auto &el : *t_obj)
    {
        if (eq(el, reflag_icase))
        {
            val |= std::regex_constants::icase;
        }

        if (eq(el, reflag_nosubs))
        {
            val |= std::regex_constants::nosubs;
        }

        if (eq(el, reflag_optimize))
        {
            val |= std::regex_constants::optimize;
        }

        if (eq(el, reflag_collate))
        {
            val |= std::regex_constants::collate;
        }

        if (eq(el, reflag_ecma_script))
        {
            val |= std::regex_constants::ECMAScript;
        }

        if (eq(el, reflag_basic))
        {
            val |= std::regex_constants::basic;
        }

        if (eq(el, reflag_extended))
        {
            val |= std::regex_constants::extended;
        }

        if (eq(el, reflag_awk))
        {
            val |= std::regex_constants::awk;
        }

        if (eq(el, reflag_grep))
        {
            val |= std::regex_constants::grep;
        }

        if (eq(el, reflag_egrep))
        {
            val |= std::regex_constants::egrep;
        }
    }

    return val;
}

}  // namespace detail

ALObjectPtr Freplace(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<4>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));

    auto repl = eval->eval(t_obj->i(2));
    AL_CHECK(assert_string(repl));

    try
    {
        std::regex_constants::match_flag_type flags = std::regex_constants::match_default;
        if (std::size(*t_obj) > 3)
        {
            auto flags_list = eval->eval(t_obj->i(3));
            AL_CHECK(assert_list(flags_list));
            flags = detail::handle_match_flags(eval_transform(eval, flags_list));
        }
        auto res = std::regex_replace(str->to_string(), detail::obj_to_regex(reg), repl->to_string(), flags);
        return make_string(res);
    }
    catch (std::regex_error &exc)
    {
        signal(re_signal, fmt::format("Re error: {}", exc.what()));
        return Qnil;
    }
}

ALObjectPtr Fmatch(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<3>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));
    std::regex_constants::match_flag_type flags = std::regex_constants::match_default;

    try
    {
        if (std::size(*t_obj) > 2)
        {
            auto flags_list = eval->eval(t_obj->i(2));
            AL_CHECK(assert_list(flags_list));
            flags = detail::handle_match_flags(eval_transform(eval, flags_list));
        }
        std::smatch match;
        auto res = std::regex_match(str->to_string(), match, detail::obj_to_regex(reg), flags);
        return res ? detail::match_to_obj(match) : Qnil;
    }
    catch (std::regex_error &exc)
    {
        signal(re_signal, fmt::format("Re error: {}", exc.what()));
        return Qnil;
    }
}

ALObjectPtr Fsearch(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<3>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));
    std::regex_constants::match_flag_type flags = std::regex_constants::match_default;


    try
    {
        if (std::size(*t_obj) > 2)
        {
            auto flags_list = eval->eval(t_obj->i(2));
            AL_CHECK(assert_list(flags_list));
            flags = detail::handle_match_flags(eval_transform(eval, flags_list));
        }
        std::smatch match;
        auto res = std::regex_search(str->to_string(), match, detail::obj_to_regex(reg), flags);
        return res ? detail::match_to_obj(match) : Qnil;
    }
    catch (std::regex_error &exc)
    {
        signal(re_signal, fmt::format("Re error: {}", exc.what()));
        return Qnil;
    }
}

ALObjectPtr Fsearch_all(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<3>(t_obj));
    auto reg = eval->eval(t_obj->i(0));

    auto str = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(str));

    std::regex_constants::match_flag_type flags = std::regex_constants::match_default;

    try
    {
        if (std::size(*t_obj) > 2)
        {
            auto flags_list = eval->eval(t_obj->i(2));
            AL_CHECK(assert_list(flags_list));
            flags = detail::handle_match_flags(eval_transform(eval, flags_list));
        }

        auto string_expr = str->to_string();
        std::string::const_iterator search_start(string_expr.cbegin());

        ALObject::list_type matches;
        std::smatch match;
        while (std::regex_search(search_start, string_expr.cend(), match, detail::obj_to_regex(reg), flags))
        {
            matches.push_back(detail::match_to_obj(match));
            search_start = match.suffix().first;
        }

        return make_list(matches);
    }
    catch (std::regex_error &exc)
    {
        signal(re_signal, fmt::format("Re error: {}", exc.what()));
        return Qnil;
    }
}


ALObjectPtr Fcompile(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_max_size<2>(t_obj));
    auto reg = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(reg));

    std::regex_constants::syntax_option_type flags = std::regex_constants::ECMAScript;

    try
    {
        if (std::size(*t_obj) > 1)
        {
            auto flags_list = eval->eval(t_obj->i(1));
            AL_CHECK(assert_list(flags_list));
            flags = detail::handle_regex_flags(flags_list);
        }

        auto id = detail::reg_registry.emplace_resource(reg->to_string(), flags)->id;
        env->defer_callback([id = id]() { detail::reg_registry.destroy_resource(id); });
        return resource_to_object(id);
    }
    catch (std::regex_error &exc)
    {
        signal(re_signal, fmt::format("Re error: {}", exc.what()));
        return Qnil;
    }
}

}  // namespace re

ALISP_EXPORT alisp::env::ModulePtr init_re(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;
    
    auto Mre    = alisp::module_init("re");
    auto re_ptr = Mre.get();

    module_doc(re_ptr, R"(The `re` module provides support for working with regular
epxression. Compiled expressions are supported and one can customized
the compiling as well as the mathcing with the expresions through
certain flags.

Internally `re` uses the C++ standard library for compiling and
matching with regex. Through flags, you can build a regex acording
one of several standards.

The symbols starting with `re-match-` modify the matching process, the
symbols starting with `re-regex-` modify the building of a regex.
)");

    module_defconst(re_ptr, "re-match-default", re::reflag_default, R"(Default flag when matching a regex.)");

    module_defconst(re_ptr,
                           "re-match-not-bol",
                           re::reflag_not_bol,
                           R"(Matching flag: The first character in [first,last) will be treated as
if it is not at the beginning of a line (i.e. ^ will not match
[first,first))");

    module_defconst(re_ptr,
                           "re-match-not-eol",
                           re::reflag_not_eol,
                           R"(Matching flag: The last character in [first,last) will be treated as
if it is not at the end of a line (i.e. $ will not match [last,last) )");

    module_defconst(
      re_ptr, "re-match-not-bow", re::reflag_not_bow, R"(Matching flag: \b" will not match [first,first))");

    module_defconst(
      re_ptr, "re-match-not-eow", re::reflag_not_eow, R"(Matching flag: "\b" will not match [last,last) )");

    module_defconst(re_ptr,
                           "re-match-any",
                           re::reflag_any,
                           R"(Matching flag: If more than one match is possible, then any match is
an acceptable result )");

    module_defconst(
      re_ptr, "re-match-not-null", re::reflag_not_null, R"(Matching flag: Do not match empty sequences )");

    module_defconst(re_ptr,
                           "re-match-continous",
                           re::reflag_continous,
                           R"(Matching flag: Only match a sub-sequence that begins at first )");

    module_defconst(re_ptr,
                           "re-match-prev-avail",
                           re::reflag_prev_avail,
                           R"(Matching flag: --first is a valid iterator position. When set, causes
match_not_bol and match_not_bow to be ignored )");

    module_defconst(re_ptr,
                           "re-match-format-default",
                           re::reflag_format_default,
                           R"(Matching flag: Use ECMAScript rules to construct strings in
re-replace (syntax documentation) )");

    module_defconst(re_ptr,
                           "re-match-format-sed",
                           re::reflag_format_sed,
                           R"(Matching flag: Use POSIX sed utility rules in re-replace. (syntax
documentation) )");

    module_defconst(re_ptr,
                           "re-match-format-no-copy",
                           re::reflag_format_no_copy,
                           R"(Matching flag: Do not copy un-matched strings to the output in
re-replace )");

    module_defconst(re_ptr,
                           "re-match-format-first-only",
                           re::reflag_format_first_only,
                           R"(Matching flag: Only replace the first match in re-replace)");


    module_defconst(re_ptr,
                           "re-regex-icase",
                           re::reflag_icase,
                           R"(Build flag: Character matching should be performed without regard to case. )");

    module_defconst(re_ptr,
                           "re-regex-nosubs",
                           re::reflag_nosubs,
                           R"(Build flag: When performing matches, all marked sub-expressions
(expr) are treated as non-marking sub-expressions (?:expr))");

    module_defconst(re_ptr,
                           "re-regex-optimize",
                           re::reflag_optimize,
                           R"(Build flag: Instructs the regular expression engine to make matching
faster, with the potential cost of making construction slower. For
example, this might mean converting a non-deterministic FSA to a
deterministic FSA.)");

    module_defconst(re_ptr,
                           "re-regex-collate",
                           re::reflag_collate,
                           R"(Build flag: Character ranges of the form "[a-b]" will be locale sensitive. )");

    module_defconst(
      re_ptr,
      "re-regex-ecma_script",
      re::reflag_ecma_script,
      R"(Build flag: Specifies that ^ shall match the beginning of a line and $ shall match the end of a line, if the ECMAScript engine is selected.)");

    module_defconst(
      re_ptr,
      "re-regex-basic",
      re::reflag_basic,
      R"(Build flag: Use the Modified [ECMAScript regular expression grammar](https://en.cppreference.com/w/cpp/regex/ecmascript))");

    module_defconst(re_ptr,
                           "re-regex-extended",
                           re::reflag_extended,
                           R"(Build flag: Use the basic POSIX regular expression grammar)");

    module_defconst(re_ptr,
                           "re-regex-awk",
                           re::reflag_awk,
                           R"(Build flag: Use the regular expression grammar used by the awk
utility in POSIX)");

    module_defconst(re_ptr,
                           "re-regex-grep",
                           re::reflag_grep,
                           R"(Build flag: Use the regular expression grammar used by the grep
utility in POSIX. This is effectively the same as the basic option
with the addition of newline '\n' as an alternation separator. )");

    module_defconst(re_ptr,
                           "re-regex-egrep",
                           re::reflag_egrep,
                           R"(Build flag: Use the regular expression grammar used by the grep
utility, with the -E option, in POSIX. This is effectively the same as
the extended option with the addition of newline '\n' as an
alternation separator in addtion to '|'.)");


    module_defun(re_ptr,
                        "re-compile",
                        &re::Fcompile,
                        R"((re-compile REGEX_STRING [BUILD_FLAGS_LIST])

Compile the regex given in the string and return a resource object to
the created regex. Optionaly, build flags can be passed through the
`BUILD_FLAGS_LIST` list.
)");


    module_defun(re_ptr,
                        "re-match",
                        &re::Fmatch,
                        R"((match [REGEX|STRING] STRING [MATCH_FLAGS])

Try to match the whole of string `STRING` with the given regex object
or regex-string. Return nil if the match fails and return a list of
the match result if the match succeeds. The first element of the list
will be the whole match, subsequent elements will correspond to the
matched groups.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.
)");

    module_defun(re_ptr,
                        "re-search",
                        &re::Fsearch,
                        R"((re-search [REGEX|STRING] STRING [MATCH_FLAGS])

Search for matching substring in `STRING` with the regex object or
regex-string. In contrast to `re-match`, this functions does not try
to match the whole string but find a part of the string that matches
the regex. Return a list with the resutls of the searching.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.
)");

    module_defun(re_ptr,
                        "re-search-all",
                        &re::Fsearch_all,
                        R"((re-search-all [REGEX|STRING] STRING [MATCH_FLAGS])

Search for all the matches of a regexc in a string. This function is
like applying re-serach several times and finding all the matches of
the regex in a given string. Return a list of lists that are the
results of the individual matches.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.)");

    module_defun(re_ptr,
                        "re-replace",
                        &re::Freplace,
                        R"((re-replace [REGEX|STRING] STRING REPLACEMENT [MATCH_FLAGS])

Try matching a part of `STRING` with the regex object or regex-string
and replace it with `REPLACEMENT`. Return the new string.

Optional flags for the mathing can be passed throught the `MATCH_FLAGS` list.
)");


    return Mre;
}
