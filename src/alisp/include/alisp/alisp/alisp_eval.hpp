#pragma once


#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility/zipping.hpp"


namespace alisp
{

namespace eval {


class Evaluator
{
  private:

    env::Environment &env;

  public:
    Evaluator(env::Environment &env_) : env(env_)
    {}

    static bool is_falsy(ALObject* obj)
    {
        if(obj == Qnil) return true;

        if(obj->type() == ALObjectType::LIST) return obj->length() == 0;
        if(obj->type() == ALObjectType::STRING_VALUE) return obj->to_string().empty();
        if(obj->type() == ALObjectType::INT_VALUE) return obj->to_int() == 0;
        if(obj->type() == ALObjectType::REAL_VALUE) return obj->to_real() == 0.0;

        return false;
				
    }

    static bool is_truthy(ALObject* obj)
    {
        return !is_falsy(obj);
    }

    ALObject* eval(ALObject* obj)
    {
        if (is_falsy(obj)) return obj;
        
        switch (obj->type()) {
          case ALObjectType::STRING_VALUE :
          case ALObjectType::REAL_VALUE :
          case ALObjectType::INT_VALUE : {
              return obj;
          }

          case ALObjectType::SYMBOL : {
              auto cell = env.find(obj);
              // TODO : fix here
              if (cell->type() == ALCellType::VALUE) {
                  return cell->value();
              } else if (cell->type() == ALCellType::FUNCTION) {
                  return make_string("Defined function");
              } else if (cell->type() == ALCellType::MACRO) {
                  return make_string("Macro");
              } else if (cell->type() == ALCellType::PRIMITIVE) {
                  return make_string("Primitive function"); 
              }
              return make_string("unknown");

          }

				case ALObjectType::LIST : {

					auto head = obj->i(0);
					auto func = env.find(head);
              
					if (func->type() == ALCellType::PRIMITIVE) {
						env::detail::FunctionCall{env};
						return func->prim()(splice(obj, 1), &env, this);
					} else if (func->type() == ALCellType::FUNCTION) {
						env::detail::FunctionCall{env};						
						return eval_function(func, splice(obj, 1));
					}
              
					throw std::runtime_error("Head of a list must be bound to function");
              
              break;
          }

          default:
              break;
        }


        return nullptr;
    }


    ALObject* eval_function(ALCell* func, ALObject* args)
    {
        // TODO : checks here
        auto[params, body] = func->callable();

        // TODO : handling arguments here

        if (params != Qnil) {
            
            if (std::size(params->children()) != std::size(args->children())){
                throw std::runtime_error("Arguments do not match");
            }
            
            const auto fun =
                [&](ALObject* param, ALObject* arg){
                    auto eval_arg = this->eval(arg);
                    auto new_cel = new ALCell("args");
                    new_cel->make_value(eval_arg);
                    this->env.put(param, new_cel);
                };
        
            utility::zip(fun, std::begin(params->children()), std::end(params->children()), std::begin(args->children()));
        }
        
        ALObject* res = nullptr;
        for (auto child : body->children()) {
            res = eval(child);
        }
        return res;

    }
};


}


}
