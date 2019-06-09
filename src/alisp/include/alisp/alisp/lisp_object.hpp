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

enum class ObjectType
{
    SYMBOL,
    LIST,
    INT,
    REAL,
    STRING,
    
    CELL,
    
    PROCEDURE,
    PRIMITIVE
};



struct Object;
struct Cell
{
    Object* con;
    Object* cdr;
};



class Env;
class Args;

typedef Object *Procedure(Env* env, Args* args);

struct Object
{
    ObjectType type;
    std::variant<int,
                 float,
                 std::string,
                 std::vector<Object*>, 
                 Cell
                 > content;


    
    Object(ObjectType type_):type(type_),content(){};
    Object():content(){};
};


void printObject(Object* obj)
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
          for(auto* o :  std::get<std::vector<Object*>>(obj->content))
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
