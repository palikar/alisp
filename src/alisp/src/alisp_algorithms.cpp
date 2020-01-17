#include <algorithm>
#include <string>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{

ALObjectPtr Fslice(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fsort(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fzip(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    return Qnil;
}

ALObjectPtr Ffilter(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fany(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fall(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}



}
