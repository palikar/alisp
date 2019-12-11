#pragma once

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility.hpp"


namespace alisp
{

namespace eval {



class Evaluator
{
  private:

    env::Environment &env;

  public:
    Evaluator(env::Environment &env_);

    ALObject* eval(ALObject* obj);
    ALObject* eval_function(ALCell* func, ALObject* args);

    template<bool evaluation = true>
    void handle_argument_bindings(ALObject* params, ALObject* args);
    
    void put_argument(ALObject* param, ALObject* arg);

    
    ALObject* operator()(ALObject* obj) { return eval(obj); }
};


}
}
