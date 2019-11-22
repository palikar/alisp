#pragma once


#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"


namespace alisp
{


template <typename Handler, typename T>
class Evaluator
{
  private:

    Handler &error_handler;
    
  public:
    Evaluator(Handler &error_handler_) :error_handler(error_handler_)
    {}

    ALObject* eval(ALObject* obj)
	{
		
        switch (obj->type) {
          case ObjectType::STRING :
		  case ObjectType::REAL :
          case ObjectType::INT : {
              
          }
          case ObjectType::SYMBOL : {              
          }
          case ObjectType::LIST : {
              break;
          }
          default:
              break;
        }
	}
};


}
