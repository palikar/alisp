#include "alisp/alisp/alisp_object.hpp"

namespace alisp
{
namespace util
{

void printObject(ALObject* obj)
{
    switch (obj->type) {

      
      case ALObjectType::STRING :{
          std::cout << '"' << std::get<std::string>(obj->content) << "\" ";
          break;
      }

      case ALObjectType::PWORD:
      case ALObjectType::ID : {
          std::cout << std::get<std::string>(obj->content) << " ";
          break;
      }

      
      case ALObjectType::REAL : {
          std::cout << std::get<float>(obj->content) << " ";
          break;
      }
          
      case ALObjectType::INT : {
          std::cout << std::get<int>(obj->content) << " ";
          break;
      }

      case ALObjectType::PLIST:
      case ALObjectType::LIST : {
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
}
