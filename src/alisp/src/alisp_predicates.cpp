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


ALObject* Fpsym(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return psym(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObject* Fplist(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return plist(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObject* Fpint(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return pint(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObject* Fpreal(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return preal(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObject* Fpstring(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return pstring(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObject* Fpfunction(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return pfunction(evl->eval(obj->i(0))) ? Qt : Qnil;
}


}
