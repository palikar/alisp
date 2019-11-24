#include "alisp/alisp/alisp_env.hpp"
#include "alisp/utility/macros.hpp"


namespace alisp
{



ALObject* Fquote(ALObject* obj, env::Environment*)
{
    return obj->i(1);
}

ALObject* Fdefun(ALObject* obj, env::Environment* env)
{
    auto new_fun = new ALCell(obj->i(0)->to_string());
    new_fun->make_function(obj->i(1), splice(obj, 2));
    env->put(obj->i(0), new_fun);
    return Qt;
}

ALObject* Fsetq(ALObject* obj, env::Environment* env)
{
    auto new_var = new ALCell(obj->i(0)->to_string());
    new_var->make_value(obj->i(1));
    env->put(obj->i(0), new_var);
    return Qt;
}

ALObject* Fprint(ALObject* obj, env::Environment* env)
{
    const auto fun =
        [](ALObject* t_obj){
            
            if(t_obj->type() == ALObjectType::STRING_VALUE || t_obj->type() == ALObjectType::SYMBOL) {
                std::cout << t_obj->to_string() << "\n";
                return Qt;
            } else if(t_obj->type() == ALObjectType::INT_VALUE) {                
                std::cout << t_obj->to_int() << "\n";
                return Qt;
            }  else if(t_obj->type() == ALObjectType::REAL_VALUE) {
                std::cout << t_obj->to_real() << "\n";
                return Qt;
            }
            return Qnil;
        };
    
    if(obj->i(0)->type() == ALObjectType::SYMBOL) {
        auto ob = env->find(obj->i(0));
        // TODO: check here
        return fun(ob->value());
    }

    if(obj->i(0)->type() == ALObjectType::LIST) {
        return Qnil;
    }

    return fun(obj->i(0));
}

ALObject* Fif(ALObject* , env::Environment*)
{
    return nullptr;
}


}
