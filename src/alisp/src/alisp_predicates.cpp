#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"

#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


ALObjectPtr Fpsym(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return psym(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fplist(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return plist(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpint(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return pint(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpreal(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return preal(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpstring(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return pstring(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpfunction(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    return pfunction(evl->eval(obj->i(0))) ? Qt : Qnil;
}


}
