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

ALObject* Fdefvar(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    assert_size<2>(obj);
    assert_symbol(obj->i(0));

    env->define_variable(obj->i(0), obj->i(1));

    return Qt;
}

ALObject* Fdefun(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    assert_symbol(obj->i(0));
    assert_list(obj->i(1));
    
    
    env->define_function(obj->i(0), obj->i(1), splice(obj, 2));
    return Qt;
}

ALObject* Fdefmacro(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    assert_symbol(obj->i(0));
    assert_list(obj->i(1));
    
    env->define_macro(obj->i(0), obj->i(1), splice(obj, 2));
    
    return Qt;
}

ALObject* Flambda(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    assert_symbol(obj->i(0));
    assert_list(obj->i(1));
    
    auto new_lambda = make_object(obj->i(1), splice(obj, 2));
    new_lambda->set_function_flag();
    
    return new_lambda;
}

ALObject* Fsetq(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_size<2>(obj);
    assert_symbol(obj->i(0));

    auto new_val = evl->eval(obj->i(1));
    env->update(obj->i(0), new_val);
    return Qt;
    
}

ALObject* Fset(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_size<2>(obj);

    auto sym = evl->eval(obj->i(0));
    
    assert_symbol(sym);

    auto new_val = evl->eval(obj->i(1));
    env->update(sym, new_val);
    return Qt;    
}

ALObject* Fquote(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);   
    return obj->i(0);
}

ALObject* Ffunction(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);   
    return obj->i(0);
}

ALObject* Fif(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<2>(obj);

    if (is_truthy(evl->eval(obj->i(0)))) {
        return evl->eval(obj->i(1));
    } else if (obj->length() == 3) {
        return evl->eval(obj->i(2));
    } else {
        return Qnil;
    }
}
ALObject* Fwhile(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<1>(obj);
    
    while (is_truthy(evl->eval(obj->i(0)))) {
        eval_list(evl, obj, 1);
    }
    return Qt;
}

ALObject* Fwhen(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<2>(obj);

    if (is_truthy(evl->eval(obj->i(0)))) {
        return eval_list(evl, obj, 1);;
    }
    return Qnil;
}

ALObject* Funless(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<2>(obj);

    if (!is_truthy(evl->eval(obj->i(0)))) {
        return eval_list(evl, obj, 1);;
    }
    return Qnil;
}

ALObject* Fdolist(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    
    assert_min_size<1>(obj);

    auto var_and_list = obj->i(0);
    auto bound_sym = var_and_list->i(0);
    auto list = evl->eval(var_and_list->i(1));

    env::detail::ScopePushPop spp{*env};

    env->put(bound_sym, Qnil);
    
    for (auto list_element : list->children())
    {
        env->update(bound_sym, list_element);

        eval_list(evl, obj, 1);
    }
    
    return Qt;

}

ALObject* Fcond(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_list(obj);

    for (auto condition : obj->children())
    {
        if (is_truthy(evl->eval(condition->i(0))))
        {
            return eval_list(evl, condition, 1);
        }
    }
    return Qnil;
}

ALObject* Fsignal(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<2>(obj);

    auto sym = evl->eval(obj->i(0));
    auto data = evl->eval(obj->i(1));

    assert_symbol(sym);
    assert_list(sym);

    throw signal_exception(sym, data);

    return Qt;
}

ALObject* Ffuncall(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(obj);

    auto fun_obj = eval->eval(obj->i(0));
    auto args = eval_transform(eval, splice(obj, 1));
    
    return eval->apply_function(fun_obj, args);

}

ALObject* Fprogn(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    return eval_list(evl, obj, 1);
}

ALObject* Flet(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_min_size<1>(obj);
    assert_list(obj->i(0));
    
    env::detail::ScopePushPop spp{*env};

    auto varlist = obj->i(0);

    std::vector<std::pair<ALObject*,ALObject*>> cells;
    cells.reserve(std::size(varlist->children()));

    for (auto var : varlist->children()) {
        if (plist(var)) {
            assert_size<2>(var);
            cells.push_back({var->i(0), evl->eval(var->i(1))});
        } else {
            assert_symbol(var);
            cells.push_back({var, Qnil});
        }
        
    }

    for (auto[ob, cell] : cells) {
        env->put(ob, cell);
    }

    return eval_list(evl, obj, 1);
}

ALObject* Fletx(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_min_size<1>(obj);
    assert_list(obj->i(0));
    
    env::detail::ScopePushPop spp{*env};

    auto varlist = obj->i(0);
    for (auto var : varlist->children()) {
        
        if (plist(var)) {
            assert_size<2>(var);
            env->put(var->i(0), evl->eval(var->i(1)));
        } else {
            assert_symbol(var);
            env->put(var, Qnil);
        }
        
    }

    return eval_list(evl, obj, 1);
}

ALObject* Fexit(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    auto val = evl->eval(obj->i(0));
    assert_int(val);
    exit(static_cast<int>(val->to_int()));
    return Qnil;
}

}
