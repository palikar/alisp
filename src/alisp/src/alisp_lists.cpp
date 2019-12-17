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



ALObject* Fcar(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    return list->i(0);
}


ALObject* Fcons(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    return splice(list, 1);
}

ALObject* Fhead(ALObject* obj, env::Environment* env, eval::Evaluator* eval)
{
    return Fcar(obj, env, eval);
}


ALObject* Ftail(ALObject* obj, env::Environment* env, eval::Evaluator* eval)
{
    return Fcons(obj, env, eval);
}

ALObject* Flast(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    return list->i(list->length() - 1);
}


ALObject* Finit(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    return splice(list, 0, static_cast<ALObject::list_type::difference_type>(list->length() - 1));
}


ALObject* Fput(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    auto element = eval->eval(obj->i(2));
    
    list->children().push_back(element);
    
    return list;
}



}
