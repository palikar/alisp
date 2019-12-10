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


void put_argument(ALObject* param, ALObject* arg)
{
    // auto eval_arg = this->eval(arg);  
    auto new_cel = new ALCell("args");
    new_cel->make_value(arg);
    this->env.put(param, new_cel);

}

void Evaluator::better_handle_argument_bindings(ALObject* params, ALObject* args)
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

    auto eval_args = eval_transform(this, args);
    
    auto next_argument = std::begin(eval_args->children());
    auto next_param = std::begin(params->children());

    auto end_argument = std::end(eval_args->children());
    auto end_param = std::end(params->children());

    size_t arg_cnt = args->length();

    size_t index = 0;
    bool rest = false;
    bool opt = false;
    bool prev_opt_or_rest = false;

    while (next_param != end_param)
    {
        if ( *next_param == Qoptional)
        {
            opt = true;
            prev_opt_or_rest = true;
            std::next(next_argument);
            continue;
        }
        else if ( *next_param == Qrest)
        {
            rest = true;
            prev_opt_or_rest = true;
            std::next(next_argument);
            continue;
        }
        else
        {
            
            if(rest)
            {
                put_argument(next_param, splice(eval_args, index));
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
            std::next(next_argument);
            std::next(next_param);
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

    
    auto opt = std::find(std::begin(params->children()), std::end(params->children()), Qoptional);
    auto rest = std::find(std::begin(params->children()), std::end(params->children()), Qrest);

    if ( opt == std::end(params->children()) && args->length() != params->length() )
    {
        // TODO: Better error here
        throw std::runtime_error("Arguments do not match");
    }


    auto next_argument = std::begin(args->children());
    auto next_param = std::begin(params->children());

    auto end_argument = std::end(args->children());
    auto end_param = std::end(params->children());

    while (next_param != opt)
    {
        auto eval_arg = this->eval(*next_argument);  
        auto new_cel = new ALCell("args");
        new_cel->make_value(eval_arg);
        this->env.put(*next_param, new_cel);

        std::next(next_argument);
        std::next(next_param);
    }

    if ( next_param == end_param ) return;
    
    std::next(next_param);


    while (next_param != rest && next_argument != end_argument)
    {

        auto eval_arg = this->eval(*next_argument);  
        auto new_cel = new ALCell("opt_args");
        new_cel->make_value(eval_arg);
        this->env.put(*next_param, new_cel);

        std::next(next_argument);
        std::next(next_param);
    }

    if ( next_argument == end_argument )
    {
        
        while (next_param != rest)
        {
            put_argument(*next_param, Qnil);
            std::next(next_param);
        }
        
        std::next(next_param);

        while (next_param != end_param)
        {
            put_argument(*next_param, Qnil);
            std::next(next_param);
        }

        return;
    }

    if ( next_param == end_param ) { return; }
    
    std::next(next_param);

    if (next_param != end_param)
    {
        if ( next_argument == std::end(args) ) { return };
        
        std::vector<ALObject*> rest_args;

        while (next_argument != end_argument)
        {
            rest_args.push_back(eval(*next_argument));
            std::next(next_argument);
        }

        auto new_cel = new ALCell("args");
            new_cel->make_value(make_object(rest_args));
            this->env.put(*next_param, new_cel);        
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

              auto head = obj->index(0);
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

        handle_argument_bindings(params, args);    
        
        ALObject* res = nullptr;
        for (auto child : body->children()) {
            res = eval(child);
        }
        return res;

    }



}

}
