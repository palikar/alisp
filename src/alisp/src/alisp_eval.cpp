
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility.hpp"


namespace alisp
{

namespace eval
{


Evaluator::Evaluator(env::Environment &env_) : env(env_) {}

                                    
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

              // env::detail::FunctionCall{env};
                  
              return func->prim()(splice(obj, 1), &env, this);
          } else if (func->type() == ALCellType::FUNCTION) {
              env::detail::FunctionCall fc{env};
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


ALObject* Evaluator::eval_function(ALCell* func, ALObject* args)
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



}

}
