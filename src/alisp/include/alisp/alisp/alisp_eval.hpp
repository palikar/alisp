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
    void handle_argument_bindings(ALObject* params, ALObject* args);
    
};


}
}
