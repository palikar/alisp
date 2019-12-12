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



ALObject* Fmapc(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);

    auto fun_obj = eval->eval(obj->i(0));
    auto list = eval->eval(obj->i(1));

    for (auto el : list->children()) {
        eval->handle_lambda(fun_obj, make_list(el));
    }

    return Qt;
}


}
