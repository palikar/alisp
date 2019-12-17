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

ALObject* Fprint(ALObject* t_obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(t_obj);

    for (auto child : t_obj->children())
    {
        auto val = eval->eval(child);
        make_visit(val,
                   type(ALObjectType::INT_VALUE ) >  [](ALObject* obj) { std::cout << obj->to_int(); },
                   type(ALObjectType::REAL_VALUE ) >  [](ALObject* obj) { std::cout << obj->to_real(); },
                   type(ALObjectType::STRING_VALUE ) >  [](ALObject* obj) { std::cout << obj->to_string(); },
                   type(ALObjectType::SYMBOL ) >  [](ALObject* obj) { std::cout << obj->to_string(); }
            );

    }    
    
    return Qt;

}

ALObject* Fprintln(ALObject* t_obj, env::Environment* env, eval::Evaluator* eval)
{
    Fprint(t_obj, env, eval);
    std::cout << '\n';
    return Qt;
}

ALObject* Fdump(ALObject* t_obj, env::Environment*, eval::Evaluator* eval)
{
    std::cout << dump(eval->eval(t_obj->i(0))) << "\n";
    return Qt;
}


ALObject* Fdumpstack(ALObject*, env::Environment* env, eval::Evaluator*)
{
    env->dump();
    return Qt;
}


}
