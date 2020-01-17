#include <algorithm>
#include <string>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{


ALObjectPtr Fint_parse(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    auto str_1 = eval->eval(obj->i(0));
    assert_string(str_1);
    try {
        return make_int(std::stoi(str_1->to_string()));
    } catch(std::invalid_argument& ex) {
        throw eval_error("The argument is not a valid integer value.");
    } catch(std::out_of_range& ex) {
        throw eval_error("The given integer value is too big.");
    }
    return Qnil;
}


ALObjectPtr Ffloat_parse(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    auto str_1 = eval->eval(obj->i(0));
    assert_string(str_1);
    try {
        return make_double(std::stod(str_1->to_string()));
    } catch(std::invalid_argument& ex) {
        throw eval_error("The argument is not a valid float value.");
    } catch(std::out_of_range& ex) {
        throw eval_error("The given float value is too big.");
    }
    return Qnil;
}


ALObjectPtr Fto_string(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);

    return make_visit(eval->eval(obj),
                      type(ALObjectType::INT_VALUE ) >>=  [](ALObjectPtr obj)    { return make_string(std::to_string(obj->to_int())); },
                      type(ALObjectType::REAL_VALUE ) >>=  [](ALObjectPtr obj)   { return make_string(std::to_string(obj->to_real())); },
                      type(ALObjectType::STRING_VALUE ) >>=  [](ALObjectPtr obj) { return make_string(obj->to_string()); },
                      type(ALObjectType::SYMBOL ) >>=  [](ALObjectPtr obj)       { return make_string(obj->to_string()); },
                      is_function() >>=  [](ALObjectPtr obj)                     { return make_string(obj->get_prop("--name--")); },
                      is_char() >>=  [](ALObjectPtr obj)                         { return make_string(char(obj->to_int())); },
                      any_pattern() >>=  [](ALObjectPtr)                         { return Qnil;}
                      
        );
}


}
