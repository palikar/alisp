#include <algorithm>
#include <string>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{

ALObjectPtr Fstring_contains(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}


ALObjectPtr Fstring_endswith(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fstring_startswtih(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fstring_length(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fstring_capitalize(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
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
