#include <algorithm>
#include <string>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{


ALObjectPtr Fint_parse(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}


ALObjectPtr Ffloat_parse(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}


ALObjectPtr Fto_string(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);

    return Qnil;
}


}
