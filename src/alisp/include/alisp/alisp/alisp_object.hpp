#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <variant>
#include <optional>
#include <functional>

namespace alisp
{

namespace inner
{

		template<typename ...Ts>
		struct Visitor : Ts...
		{
			Visitor(const Ts&... args) : Ts(args)...
			{
			}
		};

		template<typename ...Ts>
		auto make_visitor(Ts... callable)
		{
		return Visitor<Ts...>(callable...);
		}

	}

	
	


enum class ObjectType
{
	  ID,
    LIST,
    INT,
    REAL,
    STRING,
    CELL,
    PROCEDURE,
    PRIMITIVE
};

	
class Env;
class Args;

// typedef ALObject *Procedure(Env* env, Args* args);

struct Primitive
{

	void eval(std::vector<ALObject*> objs);
^
};


struct LetPrimitive : Primitive
{
	
	static void eval(std::vector<ALObject*> objs){
		// do my thing
	}

};
	
struct Procedure
{

	//body
	//parameters

};

	
struct ALObject
{
    ObjectType type;
	  std::variant<int, float, std::string, std::vector<ALObject*>,
								 Primitive*, Procedure*> content;
	
    explicit ALObject(ObjectType type_):type(type_),content(){};
    ALObject():content(){};
};



struct ALObjectStringPrinter{
	operator()(const std::string&  str)
	{
		
	}
};

	
struct ALObjectFloatPrinter{
	operator()(float f)
	{
		
	}
};


struct ALObjectIntPrinter{
	operator()(float f)
	{
		
	}
};

	
struct ALObjectListPrinter{
	operator()(const std::vector<ALObject*>&  objs)
	{
		
	}
};


struct ALObjectListPrinter{
	operator()(const Cell& cell)
	{
		
	}
};

	



	
void printObject(ALObject* obj)
{
	auto print_visitor = inner::make_visitor(ALObjectIntPrinter,
																					 ALObjectFloatPrinter,
																					 ALObjectListPrinter,
																					 ALObjectStringPrinter,
																					 ALObjectCellPrinter);
	std::visit(obj->content, print_visitor);
}

}


class Stack;
class Context;

class Evaluator
{

	Stack tack;
	Context current_context;

	ALObject* eval(ALObject* obj)
	{
		
    switch (obj->type) {

		  case ObjectType::SYMBOL : {
				//check the current context and return the thing
			}
		  case ObjectType::STRING :
		  case ObjectType::REAL :
      case ObjectType::INT : {
				
			}

      case ObjectType::LIST : {

				//evaluate the first thin -> it has to be a procedure or construct!

				
				
				break;
      }
      default:
          std::cout << "" << "\n";
          break;
    }



	}


	ALObject* operator()(std::vector<ALObject*> objs)
	{
		//1. get the first object
		//2. find the procedure
		//3. do some checks
		//4. push things on the stack
		//5. update current context
		//6. call the procedure
		
	}

	
	ALObject* operator()(Procedure proc)
	{
		// somethig is wrong here	
	}


	


}

class Context;

struct NodeBase{

	NodeBase(const NodeBase&) = delete;
	NodeBase(NodeBase&&) = delete;
	
	virtual ALObject* eval(Context con) = 0;
	virtual std::vectror<ALObject*> get_children(Context con) = 0;
};

<typename T>
struct NodeImpl : NodeBase{
	NodeImpl(..) : NodeBase()
		{}
	
	ALObject* eval(Context con) final{
		T::trace(con);
		return eval_internal(con);
	}

	ALObject* eval_internal(Context con)
		{
			return nullptr;
		};
	
	std::vector<ALObject*> children;
	
};


<typename T>
struct LetNode : NodeImpl<T> {


	//1 . check format
	
	ALObject* eval_internal(Context con)
		{

			//2. create new scope on the stack
			//3. bind things
			
			//4. evaluate the rest of the children

			return nullptr;
		};
	

};











