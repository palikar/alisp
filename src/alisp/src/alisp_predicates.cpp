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


ALObject* Fpsym(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return psym(obj) ? Qt : Qnil;
}

ALObject* Fplist(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return plist(obj) ? Qt : Qnil;
}

ALObject* Fpint(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return pint(obj) ? Qt : Qnil;
}

ALObject* Fpreal(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return preal(obj) ? Qt : Qnil;
}

ALObject* Fpstring(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return pstring(obj) ? Qt : Qnil;
}

ALObject* Fpfunction(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return pfunction(obj) ? Qt : Qnil;
}


}
