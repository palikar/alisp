#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"


#include "alisp/utility/macros.hpp"


namespace alisp
{

template<size_t N>
void assert_min_size (ALObject* obj)
{
    if(!min_list_elements(obj, N))
        throw std::runtime_error("Invalid argument. Must be list with at least " + std::to_string(N) + " elements"); 
}

template<size_t N>
void assert_max_size (ALObject* obj)
{
    if(!max_list_elements(obj, N))
        throw std::runtime_error("Invalid argument. Must be list with maximum of " + std::to_string(N) + " elements"); 
}

template<size_t N>
void assert_size (ALObject* obj)
{
    if(obj->length() != N)
        throw std::runtime_error("Invalid argument. Must be list with  " + std::to_string(N) + " elements"); 
}
    
void assert_numbers (ALObject* obj)
{
    if(!are_objects_numbers(obj))
        throw std::runtime_error("Invalid argument. The list must contain only numbers(real of int)");
}

void assert_symbol (ALObject* obj)
{
    if(!obj->is_sym()) throw std::runtime_error("Invalid argument. Object must be symbol");
}

void assert_list (ALObject* obj)
{
    if(!obj->is_list() and obj != Qnil) throw std::runtime_error("Invalid argument. Object must be list");
}

void assert_number (ALObject* obj)
{
    if(!obj->is_int() and !obj->is_real()) throw std::runtime_error("Invalid argument. Object must be a number");
}

void assert_int (ALObject* obj)
{
    if(!obj->is_int()) throw std::runtime_error("Invalid argument. Object must be an integer");
}






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

ALObject* Flambda(ALObject* obj, env::Environment*, eval::Evaluator* eval)
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




ALObject* Fmultiply(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);

    assert_numbers(eval_obj);
    
    if (are_objects_int(eval_obj)) {
        const ALObject::int_type product = reduce<false>(evl, eval_obj, MUL_OBJ_FUN, static_cast<ALObject::int_type>(1));
        return make_int(product);
    } else if (are_objects_numbers(eval_obj)){
        const ALObject::real_type product = reduce<false>(evl, eval_obj, MUL_OBJ_FUN_D, static_cast<ALObject::real_type>(1));
        return make_double(product);
    }
    return Qnil;
}

ALObject* Fplus(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);

    assert_numbers(eval_obj);
    
    if (are_objects_int(eval_obj)) {
        const ALObject::int_type sum = reduce<false>(evl, eval_obj, ADD_OBJ_FUN, static_cast<ALObject::int_type>(0));
        return make_int(sum);
    } else if (are_objects_numbers(eval_obj)){
        const ALObject::real_type sum = reduce<false>(evl, eval_obj, ADD_OBJ_FUN_D, static_cast<ALObject::real_type>(0));
        return make_double(sum);
    }
    return Qnil;

}

ALObject* Fminus(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);

    assert_numbers(eval_obj);

    if (are_objects_int(eval_obj)) {
        const ALObject::int_type sum = reduce<false>(evl, eval_obj, SUB_OBJ_FUN, eval_obj->i(0)->to_int(), 1);
        return make_int(sum);
    } else if (are_objects_numbers(eval_obj)){
        const ALObject::real_type sum = reduce<false>(evl, eval_obj, SUB_OBJ_FUN_D, eval_obj->i(0)->to_real(), 1);
        return make_double(sum);
    }
    return Qnil;

}

ALObject* Fdev(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);

    assert_numbers(eval_obj);
    
    if (are_objects_int(eval_obj)) {
        const ALObject::int_type sum = reduce<false>(evl, eval_obj, DIV_OBJ_FUN, eval_obj->i(0)->to_int(), 1);
        return make_int(sum);
    } else if (are_objects_numbers(eval_obj)){
        const ALObject::real_type sum = reduce<false>(evl, eval_obj, DIV_OBJ_FUN_D, eval_obj->i(0)->to_real(), 1);
        return make_double(sum);
    }
    return Qnil;
}

ALObject* Fand(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);
    bool sum = reduce<false>(evl, eval_obj, AND_OBJ_FUN, true);
    return sum ? Qt : Qnil;
}

ALObject* For(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto eval_obj = eval_transform(evl, obj);
    bool sum = reduce<false>(evl, eval_obj, OR_OBJ_FUN, false);
    return sum ? Qt : Qnil;
}

ALObject* Fnot(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    
    bool sum = is_truthy(evl->eval(obj->i(0)));
    return !sum ? Qt : Qnil;
}


ALObject* Fleftshift(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));

    assert_int(rhs);
    assert_int(lhs);

    return make_int(SHIFT_LEFT(lhs, rhs));
}

ALObject* Frightshift(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    assert_int(rhs);
    assert_int(lhs);
    return make_int(SHIFT_RIGHT(lhs, rhs));
}




ALObject* Flt(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));
    
    assert_number(one);
    assert_number(two);
    
    if (one->to_real() < two->to_real()) { return Qt; }
    else { return Qnil; };
}

ALObject* Fleq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);

    if (one->to_real() <= two->to_real()) return Qt;
    else return Qnil;
}

ALObject* Fgt(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);
    
    if (one->to_real() > two->to_real()) return Qt;
    else return Qnil;
}

ALObject* Fgeq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);
    
    if (one->to_real() >= two->to_real()) return Qt;
    else return Qnil;
}

ALObject* Feq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);
    
    if (one->to_real() == two->to_real()) return Qt;
    else return Qnil;
}

ALObject* Fneq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);

    if (one->to_real() != two->to_real()) return Qt;
    else return Qnil;
}

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


ALObject* Fmapc(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    
    auto fun_obj = eval->eval(obj->i(0));
    auto list = eval->eval(obj->i(1));

    for (auto el : list) {
        eval->apply_function(fun_obj, list);
    }
    
}

ALObject* Ffuncall(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(obj);

    auto fun_obj = eval->eval(obj->i(0));
    auto args = eval_transform(splice(obj, 1));
    
    eval->apply_function(fun_obj, args);

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
        std::cout << dump(var) << "\n";
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
