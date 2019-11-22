#pragma once


#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"


namespace alisp
{


	namespace eval {
		
		template <class Handler>
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
					case ALObjectType::STRING :
					case ALObjectType::REAL :
					case ALObjectType::INT : {

						break;
					}
					
					case ALObjectType::SYMBOL : {

						break;
					}
					case ALObjectType::LIST : {
						break;
					}
					
					default:
						break;
					}
				}
		};


	}


}
