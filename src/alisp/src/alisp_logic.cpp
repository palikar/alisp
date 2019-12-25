#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{



ALObjectPtr Fand(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);
    bool sum = reduce<false>(evl, eval_obj, AND_OBJ_FUN, true);
    return sum ? Qt : Qnil;
}

ALObjectPtr For(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);
    bool sum = reduce<false>(evl, eval_obj, OR_OBJ_FUN, false);
    return sum ? Qt : Qnil;
}

ALObjectPtr Fnot(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    
    bool sum = is_truthy(evl->eval(obj->i(0)));
    return !sum ? Qt : Qnil;
}





}
