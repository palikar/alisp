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

enum class ALObjectType
{
    ID,
    INT,
    REAL,
    STRING,
    LIST,

    PLIST,
    PWORD
};

enum class ALCellType
{
    VALUE,
    PRIMITIVE,
    PROCEDUREx
};

struct ALObject
{
    ALObjectType type;
    std::variant<int, float, std::string, std::vector<ALObject*>> content;
    explicit ALObject(ALObjectType type_) : type(type_), content(){};
};

struct Primitive;
struct Procedure;

struct ALCell
{
    ALCellType type;
    
    union{
        Procedure *proc;
        Primitive *prim;
        ALObject  *obj;
    };
    
    ALCell(ALCellType type_) : type(type_){};
};

namespace util
{
void printObject(ALObject* obj);
}

}















