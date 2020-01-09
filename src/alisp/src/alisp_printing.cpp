#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{

ALObjectPtr Fprint(ALObjectPtr t_obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(t_obj);

    for (auto child : *t_obj)
    {
        auto val = eval->eval(child);
        
        make_visit(val,
                   type(ALObjectType::INT_VALUE ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_int(); },
                   type(ALObjectType::REAL_VALUE ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_real(); },
                   type(ALObjectType::STRING_VALUE ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_string(); },
                   type(ALObjectType::SYMBOL ) >>=  [](ALObjectPtr obj) { std::cout << obj->to_string(); }
            );
        
    }    
    
    return Qt;

}

ALObjectPtr Fprintln(ALObjectPtr t_obj, env::Environment* env, eval::Evaluator* eval)
{
    Fprint(t_obj, env, eval);
    std::cout << '\n';
    return Qt;
}

ALObjectPtr Fdump(ALObjectPtr t_obj, env::Environment*, eval::Evaluator* eval)
{
    std::cout << dump(eval->eval(t_obj->i(0))) << "\n";
    return Qt;
}


ALObjectPtr Fdumpstack(ALObjectPtr, env::Environment* env, eval::Evaluator*)
{
    env->stack_dump();
    return Qt;
}


ALObjectPtr Fdumpcallstack(ALObjectPtr, env::Environment* env, eval::Evaluator*)
{
    env->callstack_dump();
    return Qt;
}



}
