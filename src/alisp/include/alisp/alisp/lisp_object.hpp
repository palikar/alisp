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


void printObject(Object* obj);

}
