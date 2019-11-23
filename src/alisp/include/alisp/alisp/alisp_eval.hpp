#pragma once


#include "alisp/alisp/alisp_common.hpp"


namespace alisp
{


namespace eval {

template <typename Environment>
class Evaluator
{
  private:

    Environment &env;

  public:
    Evaluator(Environment &env_) : env(env_)
    {}

    ALObject* eval(ALObject* obj)
    {

        switch (obj->type()) {
          case ALObjectType::STRING_VALUE :
          case ALObjectType::REAL_VALUE :
          case ALObjectType::INT_VALUE : {
              return obj;
          }

          case ALObjectType::SYMBOL : {
              auto cell = env.find(obj);

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

              if (func->type() == ALCellType::PRIMITIVE)
              {
                  return func->prim()(obj, &env);
              }
              
              break;
          }

          default:
              break;
        }


        return nullptr;
    }
};


}


}
