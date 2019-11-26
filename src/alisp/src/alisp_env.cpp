#include <algorithm>

#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"

#include "alisp/utility/macros.hpp"

namespace alisp
{

ALObject* Fdefvar(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    auto new_var = new ALCell(obj->i(0)->to_string());
    new_var->make_value(obj->i(1));
    env->put(obj->i(0), new_var);
    return Qt;
}

ALObject* Fquote(ALObject* obj, env::Environment*, eval::Evaluator*)
{
    return obj->i(0);
}


ALObject* Fdefun(ALObject* obj, env::Environment* env, eval::Evaluator*)
{
    auto new_fun = new ALCell(obj->i(0)->to_string());
    new_fun->make_function(obj->i(1), splice(obj, 2));
    env->put(obj->i(0), new_fun);
    return Qt;
}


ALObject* Fsetq(ALObject* obj, env::Environment* env, eval::Evaluator* evl)
{
    auto new_var = new ALCell(obj->i(0)->to_string());
    new_var->make_value(evl->eval(obj->i(1)));
    env->put(obj->i(0), new_var);
    return Qt;
}


ALObject* Fprint(ALObject* obj, env::Environment*, eval::Evaluator* eval)
{
    const auto fun =
        [](ALObject* t_obj){

            if(t_obj->type() == ALObjectType::STRING_VALUE || t_obj->type() == ALObjectType::SYMBOL) {
                std::cout << t_obj->to_string() << "\n";
                return Qt;
            } else if(t_obj->type() == ALObjectType::INT_VALUE) {
                std::cout << t_obj->to_int() << "\n";
                return Qt;
            } else if(t_obj->type() == ALObjectType::REAL_VALUE) {
                std::cout << t_obj->to_real() << "\n";
                return Qt;
            }


            return Qnil;
        };

    auto val = eval->eval(obj->i(0));
    return fun(val);
}


ALObject* Fif(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    // TODO: sanity checks

    if (eval::Evaluator::is_truthy(evl->eval(obj->i(0)))) {
        return evl->eval(obj->i(1));
    } else if (obj->length() == 3) {
        return evl->eval(obj->i(2));
    } else {
        return Qnil;
    }
}

ALObject* Fwhile(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto fun = [&](auto& o){ evl->eval(o); };
    while (eval::Evaluator::is_truthy(evl->eval(obj->i(0)))) {
        std::for_each(std::next(std::begin(obj->children())), std::end(obj->children()), fun);
    }
    return Qt;
}

ALObject* Fmultiply(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    int64_t product = 1;

    for(auto o : obj->children())
    {
        product *= evl->eval(o)->to_int();
    }

    return make_int(product);
}


ALObject* Fplus(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    int64_t sum = 0;

    for(auto o : obj->children())
    {
        sum += evl->eval(o)->to_int();
    }

    return make_int(sum);
}


ALObject* Fminus(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    int64_t sub = evl->eval(obj->i(0))->to_int();

    const auto fun =
        [&](auto& o){
            sub -= evl->eval(o)->to_int();
        };
    std::for_each(std::next(std::begin(obj->children())), std::end(obj->children()), fun);

    return make_int(sub);
}

ALObject* Fdev(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    int64_t sub = evl->eval(obj->i(0))->to_int();

    const auto fun =
        [&](auto& o){
            sub /= evl->eval(o)->to_int();
        };
    std::for_each(std::next(std::begin(obj->children())), std::end(obj->children()), fun);

    return make_int(sub);
}


ALObject* Flt(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto one = evl->eval(obj->i(0))->to_int();
    const auto two = evl->eval(obj->i(1))->to_int();
    if (one < two) return Qt;
    else return Qnil;
}


ALObject* Fleq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto one = evl->eval(obj->i(0))->to_int();
    const auto two = evl->eval(obj->i(1))->to_int();
    if (one <= two) return Qt;
    else return Qnil;
}


ALObject* Fgt(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto one = evl->eval(obj->i(0))->to_int();
    const auto two = evl->eval(obj->i(1))->to_int();
    if (one > two) return Qt;
    else return Qnil;
}


ALObject* Fgeq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto one = evl->eval(obj->i(0))->to_int();
    const auto two = evl->eval(obj->i(1))->to_int();
    if (one >= two) return Qt;
    else return Qnil;
}

ALObject* Feq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto one = evl->eval(obj->i(0))->to_int();
    const auto two = evl->eval(obj->i(1))->to_int();
    if (one == two) return Qt;
    else return Qnil;
}

ALObject* Fneq(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    const auto one = evl->eval(obj->i(0))->to_int();
    const auto two = evl->eval(obj->i(1))->to_int();
    if (one != two) return Qt;
    else return Qnil;
}


ALObject* Fprogn(ALObject* obj, env::Environment*, eval::Evaluator* evl)
{
    ALObject* res;
    for(auto o : obj->children()){ res = evl->eval(o); }
    return res;
}




}
