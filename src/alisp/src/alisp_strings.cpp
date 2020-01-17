#include <algorithm>
#include <string>
#include <cctype>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{

ALObjectPtr Fstring_contains(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);

    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval-eval(obj->i(1));

    assert_string(str_1);
    assert_string(str_2);
    
    if (str_1->to_string().find(str_2->to_string()) != std::string::npos) {
        return Qt;
    }
    return Qnil;
}


ALObjectPtr Fstring_endswith(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);

    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval-eval(obj->i(1));

    assert_string(str_1);
    assert_string(str_2);
    
    if (utility::ends_with(str_1->to_string(), str_2->to_string())) {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_startswtih(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);

    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval-eval(obj->i(1));

    assert_string(str_1);
    assert_string(str_2);
    
    if (utility::starts_with(str_1->to_string(), str_2->to_string())) {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_length(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_string(str);
    return make_int(std::size(str->to_string()));
}

ALObjectPtr Fstring_capitalize(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_string(str);
    auto& s = str->to_string();
    s[0] = std::toupper(s[0]);
    return str;
}

ALObjectPtr Fstring_isalpha(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_isdecimal(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_isdigit(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_find(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fstring_replace(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fstring_replaceall(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fstring_split(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fstring_substring(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<3>(obj);
    return Qnil;
}

ALObjectPtr Fstring_splitlines(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_upper(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_lower(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_strip(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_join(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_min_size<1>(obj);
    return Qnil;
}


}
