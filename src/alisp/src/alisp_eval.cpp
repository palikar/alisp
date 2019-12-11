#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility.hpp"

#include <algorithm>

namespace alisp
{

namespace eval
{


Evaluator::Evaluator(env::Environment &env_) : env(env_) {}


void Evaluator::put_argument(ALObject* param, ALObject* arg)
{
    this->env.put(param, arg);
}

template<bool evaluation>
void Evaluator::handle_argument_bindings(ALObject* params, ALObject* args)
{

    if (params->length() == 0 && args->length() != 0)
    {
        // TODO: Better error here
        throw std::runtime_error("Arguments do not match");
    }

    if (args->length() != 0 && params->length() == 0 )
    {
        // TODO: Better error here
        throw std::runtime_error("Arguments do not match");
    }

    if (args->length() == 0 && params->length() == 0 ) { return; }

    auto eval_args =
        [&](){
            if constexpr (evaluation) {
                return eval_transform(this, args);
            } else {
                return args;
            }
        }();

    

    auto next_argument = std::begin(eval_args->children());
    auto next_param = std::begin(params->children());

    auto end_param = std::end(params->children());

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
                 // TODO: Better error here
                 throw std::runtime_error("Arguments do not match");
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
        // TODO: Better error here
        throw std::runtime_error("Arguments do not match");
    }

    if (index < arg_cnt)
    {
        // TODO: Better error here
        throw std::runtime_error("Arguments do not match");
    }

}

ALObject* Evaluator::eval(ALObject* obj)
{
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
              throw std::runtime_error("Head of a list must be bound to function");
          }
          
          // CallTracer tracer{};
          try {

              if (func->check_prime_flag) {
              
                  // TODO: Trace the stack here
                  return func->get_prime()(splice(obj, 1), &env, this);
              
              } else if (func->check_macro_flag) {

                  env::detail::FunctionCall fc{env};
                  // TODO: Trace the stack here
                  return eval(apply_function(func, splice(obj, 1)));
              
              } else {
              
                  env::detail::FunctionCall fc{env};
                  // TODO: Trace the stack here
                  return eval_function(func, splice(obj, 1));
              
              }

              
          } catch (...) {

              // tracer.dump()
              throw;
          }

          
          break;
      }

      default: break;
    }


    return nullptr;
}

ALObject* Evaluator::eval_function(ALObject* func, ALObject* args)
{
    auto[params, body] = func->get_function();
    handle_argument_bindings(params, args);
    return eval_list(this, body, 0);
}

ALObject* Evaluator::apply_function(ALObject* func, ALObject* args)
{
    auto[params, body] = func->get_function();
    handle_argument_bindings<false>(params, args);
    return eval_list(this, body, 0);
}


}

}
