#include "alisp/alisp/lisp_object.hpp"

namespace alisp
{

void printObject(ALObject* obj)
{
    switch (obj->type) {

      case ObjectType::CELL :{


          auto c = std::get<Cell>(obj->content);
          std::cout << "(";
          printObject(c.con);
          std::cout << ". ";
          printObject(c.cdr);
          std::cout << ") ";
          break;
      }

      case ObjectType::STRING :{
          std::cout << '"' << std::get<std::string>(obj->content) << "\" ";
          break;
      }
      case ObjectType::SYMBOL : {
          std::cout << std::get<std::string>(obj->content) << " ";
          break;
      }
      case ObjectType::REAL : {
          std::cout << std::get<float>(obj->content) << " ";
          break;
      }
      case ObjectType::INT : {
          std::cout << std::get<int>(obj->content) << " ";
          break;
      }

      case ObjectType::LIST : {
          std::cout << "(";
          for(auto* o :  std::get<std::vector<ALObject*>>(obj->content))
          {
              printObject(o);
          }
          std::cout << ") ";
          
          break;
      }
      default:
          std::cout << "" << "\n";
          break;
    }

}
}
