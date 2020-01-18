#include <algorithm>
#include <cmath>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"
#include "alisp/utility/math_utils.hpp"


namespace alisp
{



ALObjectPtr Fmultiply(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
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

ALObjectPtr Fplus(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
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

ALObjectPtr Fminus(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
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

ALObjectPtr Fdev(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
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

ALObjectPtr Fleftshift(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));

    assert_int(rhs);
    assert_int(lhs);

    return make_int(SHIFT_LEFT(lhs, rhs));
}

ALObjectPtr Frightshift(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    auto lhs = evl->eval(obj->i(0));
    auto rhs = evl->eval(obj->i(1));
    assert_int(rhs);
    assert_int(lhs);
    return make_int(SHIFT_RIGHT(lhs, rhs));
}

ALObjectPtr Flt(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));
    
    assert_number(one);
    assert_number(two);
    
    if (one->to_real() < two->to_real()) { return Qt; }
    else { return Qnil; };
}

ALObjectPtr Fleq(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);

    if (one->to_real() <= two->to_real()) return Qt;
    else return Qnil;
}

ALObjectPtr Fgt(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);
    
    if (one->to_real() > two->to_real()) return Qt;
    else return Qnil;
}

ALObjectPtr Fgeq(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);
    
    if (one->to_real() >= two->to_real()) return Qt;
    else return Qnil;
}

ALObjectPtr Feq(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);
    
    if (one->to_real() == two->to_real()) return Qt;
    else return Qnil;
}

ALObjectPtr Fneq(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<0>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);

    if (one->to_real() != two->to_real()) return Qt;
    else return Qnil;
}

ALObjectPtr Fmod(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<2>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_int(one);
    assert_int(two);

    auto res = one->to_int() % two->to_int();

    return make_object(res);
}

ALObjectPtr Fpow(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<2>(obj);
    
    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));

    assert_number(one);
    assert_number(two);

    return make_object(std::pow(one->to_real(), two->to_real()));
}

ALObjectPtr Fmin(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<2>(obj);

    auto eval_obj = eval_transform(eval, obj) ;
    assert_numbers(eval_obj);
    auto is_int = are_objects_int(eval_obj);
    if (is_int) {
        auto min_element = obj->i(0)->to_int();
        for (auto el : *eval_obj) {
            auto current = el->to_int();
            if (current < min_element) {
                min_element = current;
            }
        }
        return make_int(min_element);
    } else {
        
        auto min_element = obj->i(0)->to_real();
        for (auto el : *eval_obj) {
            auto current = el->to_real();
            if (current < min_element) {
                min_element = current;
            }
        }
        return make_double(min_element);
    }
}

ALObjectPtr Fmax(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<2>(obj);

    auto eval_obj = eval_transform(eval, obj);
    assert_numbers(eval_obj);
    auto is_int = are_objects_int(eval_obj);
    if (is_int) {
        auto max_element = obj->i(0)->to_int();
        for (auto el : *eval_obj) {
            auto current = el->to_int();
            if (max_element < current) {
                max_element = current;
            }
        }
        return make_int(max_element);
    } else {
        
        auto max_element = obj->i(0)->to_real();
        for (auto el : *eval_obj) {
            auto current = el->to_real();
            if (max_element < current) {
                max_element = current;
            }
        }
        return make_double(max_element);
    }
}

ALObjectPtr Fround(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<2>(obj);

    const auto one = evl->eval(obj->i(0));
    const auto two = evl->eval(obj->i(1));
    
    assert_number(one);
    assert_int(two);

    return make_double(utility::round_nplaces(one->to_real(), two->to_int()));
}


}
