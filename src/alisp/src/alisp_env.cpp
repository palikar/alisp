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
    if(!obj->is_int() or !obj->is_real()) throw std::runtime_error("Invalid argument. Object must be a number");
}

void assert_int (ALObject* obj)
{
    if(!obj->is_int()) throw std::runtime_error("Invalid argument. Object must be an integer");
}


ALObject* Fdefvar(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    assert_size<2>(obj);
    assert_symbol(obj->i(0));

    auto new_var = new ALCell(obj->i(0)->to_string());
    new_var->make_value(obj->i(1));
    env->put(obj->i(0), new_var);
    return Qt;
}

ALObject* Fquote(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    
    return obj->i(0);
}

ALObject* Fdefun(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    assert_symbol(obj->i(0));
    assert_list(obj->i(1));
    
    auto new_fun = new ALCell(obj->i(0)->to_string());
    new_fun->make_function(obj->i(1), splice(obj, 2));
    env->put(obj->i(0), new_fun);
    return Qt;
}

ALObject* Fsetq(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_size<2>(obj);
    assert_symbol(obj->i(0));
    
    auto new_var = new ALCell(obj->i(0)->to_string());
    new_var->make_value(evl->eval(obj->i(1)));
    env->put(obj->i(0), new_var);
    return Qt;
}

ALObject* Fprint(ALObject* t_obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(t_obj);
    
    auto val = eval->eval(t_obj->i(0));
    make_visit(val,
               type(ALObjectType::INT_VALUE ) >  [](ALObject* obj) { std::cout << obj->to_int() << '\n'; },
               type(ALObjectType::REAL_VALUE ) >  [](ALObject* obj) { std::cout << obj->to_real() << '\n'; },
               type(ALObjectType::STRING_VALUE ) >  [](ALObject* obj) { std::cout << obj->to_string() << '\n'; },
               type(ALObjectType::SYMBOL ) >  [](ALObject* obj) { std::cout << obj->to_string() << '\n'; }
        );

    return Qt;

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

    std::vector<std::pair<ALObject*,ALCell*>> cells;
    cells.reserve(std::size(varlist->children()));

    for (auto var : varlist->children()) {
        auto new_var = new ALCell(var->i(0)->to_string());
        new_var->make_value(evl->eval(var->i(1)));
        cells.push_back({var->i(0), new_var});
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
        auto new_var = new ALCell(var->i(0)->to_string());
        new_var->make_value(evl->eval(var->i(1)));
        env->put(var->i(0), new_var);
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
