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


}
