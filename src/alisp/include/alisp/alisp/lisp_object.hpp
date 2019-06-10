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



struct ALObject;
struct Cell
{
    ALObject* con;
    ALObject* cdr;
};



class Env;
class Args;

typedef ALObject *Procedure(Env* env, Args* args);

struct ALObject
{
    ObjectType type;
    std::variant<int,
                 float,
                 Cell,
                 std::string,
                 std::vector<ALObject*> > content;

    ALObject(ObjectType type_):type(type_),content(){};
    ALObject():content(){};
};


void printObject(ALObject* obj);

}
