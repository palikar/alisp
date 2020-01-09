#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_exception.hpp"

#include "alisp/utility.hpp"

#include <algorithm>

namespace alisp
{

namespace eval
{


void Evaluator::new_evaluation() {
    ++m_eval_depth;
    if (m_eval_depth > MAX_EAVALUATION_DEPTH) { throw eval_error("Maximum evaluation depth reached!"); }
}

void Evaluator::end_evaluation() { --m_eval_depth; }


Evaluator::Evaluator(env::Environment &env_) : env(env_) {}


void Evaluator::put_argument(ALObjectPtr param, ALObjectPtr arg)
{
    this->env.put(param, arg);
}

template<bool evaluation>
void Evaluator::handle_argument_bindings(ALObjectPtr params, ALObjectPtr args)
{

    if (params->length() == 0 && args->length() != 0)
    {
        throw argument_error("Argument\'s lengths do not match.");
    }

    if (args->length() != 0 && params->length() == 0 )
    {
        throw argument_error("Argument\'s lengths do not match.");
    }

    if (args->length() == 0 && params->length() == 0 ) { return; }


    auto eval_args = args;
    
    auto next_argument = std::begin(*eval_args);
    auto next_param = std::begin(*params);


    auto end_param = std::end(*params);

    auto arg_cnt = static_cast<ALObject::list_type::difference_type>(args->length());

    ALObject::list_type::difference_type index = 0;
    bool rest = false;
    bool opt = false;
    bool prev_opt_or_rest = false;

    while (next_param != end_param)
    {
         if ( *next_param == Qoptional)
         {
             opt = true;
             prev_opt_or_rest = true;
             next_param = std::next(next_param);
             continue;
         }
         else if ( *next_param == Qrest)
         {
             rest = true;
             prev_opt_or_rest = true;
             next_param = std::next(next_param);
             continue;
         }
         else
         {

             if(rest)
             {
                 put_argument(*next_param, splice(eval_args, index));
                 return;
             }
             else if(index < arg_cnt)
             {
                 put_argument(*next_param, *next_argument);
             }
             else if(!opt)
             {
                 throw argument_error("The function do not accept optional arguments.");
             }
             else
             {
                 put_argument(*next_param, Qnil);
             }

             ++index;
             prev_opt_or_rest = false;
             next_argument = std::next(next_argument);
             next_param = std::next(next_param);
         }

     }


    if ( prev_opt_or_rest )
    {
        throw argument_error("Parameters end with &optional or &rest.");
    }

    if (index < arg_cnt)
    {
        throw argument_error("Not enough argument for the function.");
    }

}

ALObjectPtr Evaluator::eval(ALObjectPtr obj)
{
    detail::EvalDepthTrack{*this};

    if (is_falsy(obj)) return obj;

    switch (obj->type()) {
      case ALObjectType::STRING_VALUE :
      case ALObjectType::REAL_VALUE :
      case ALObjectType::INT_VALUE : {
          return obj;
      }

      case ALObjectType::SYMBOL : {
          return env.find(obj);
      }

      case ALObjectType::LIST : {

          auto func = env.find(obj->i(0));
          if ( !func->check_function_flag() ) {
              throw eval_error("Head of a list must be bound to function");
          }


          env::detail::CallTracer tracer{env};
          tracer.function_name(obj->i(0)->to_string(), func->check_prime_flag());

          try {

              if (func->check_prime_flag()) {                  

                  // STACK_ALLOC_OBJECT(eval_obj, eval_ptr, utility::slice_view(obj->children(), 1));
                  
                  return func->get_prime()(splice(obj, 1), &env, this);
              } else if (func->check_macro_flag()) {
                  env::detail::FunctionCall fc{env};

                  // STACK_ALLOC_OBJECT(eval_obj, eval_ptr, utility::slice_view(obj->children(), 1));
                  
                  return eval(apply_function(func, splice(obj, 1)));
              } else {
                  // STACK_ALLOC_OBJECT(eval_obj, eval_ptr, utility::slice_view(obj->children(), 1));
                  
                  return eval_function(func, splice(obj, 1));
              }


          }catch (al_return&) {
              throw;
          } catch (...) {
              tracer.dump();
              throw;
          }


          break;
       }

       default:
           eval_error("Unknown object typee");
    }


     return nullptr;
}

ALObjectPtr Evaluator::eval_function(ALObjectPtr func, ALObjectPtr args)
{
    auto[params, body] = func->get_function();
    auto eval_args = eval_transform(this, args);
    try {
        env::detail::FunctionCall fc{env};
        handle_argument_bindings(params, eval_args);
        return eval_list(this, body, 0);
    } catch (al_return& ret){
        return  ret.value();
    }
    
}

ALObjectPtr Evaluator::apply_function(ALObjectPtr func, ALObjectPtr args)
{
    auto[params, body] = func->get_function();
    handle_argument_bindings<false>(params, args);
    return eval_list(this, body, 0);
}

ALObjectPtr Evaluator::handle_lambda(ALObjectPtr func, ALObjectPtr args)
{
    auto obj = func;
    if(psym(func))
    {
        obj = eval(func);
    }
    
    if (!obj->check_function_flag()) {
        throw eval_error("Cannot apply a non function object.");
    }

    env::detail::FunctionCall fc{env};
    if (obj->check_prime_flag()) {
        return obj->get_prime()(args, &env, this);
    } else {
        return apply_function(obj, args);
    }

}


}

}
