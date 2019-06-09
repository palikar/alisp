#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <variant>
#include "numler/alisp/common.hpp"


namespace alisp
{


enum class ObjectType
{
    SYMBOL,
    LIST,
    INT,
    REAL,
    CELL,
    STRING,
    PROCEDURE,
    PRIMITIVE
};

struct Object
{

    ObjectType type;

    Object():content()
    {};
    std::variant<int, float, std::string, std::vector<Object*>> content;
};


void printObject(Object* obj)
{
    switch (obj->type) {
      case ObjectType::STRING:{
          std::cout << '"' << std::get<std::string>(obj->content) << "\" ";
          break;
      }
      case ObjectType::SYMBOL: {
          std::cout << std::get<std::string>(obj->content) << " ";
          break;
      }
      case ObjectType::REAL: {
          std::cout << std::get<float>(obj->content) << " ";
          break;
      }
      case ObjectType::INT: {
          std::cout << std::get<int>(obj->content) << " ";
          break;
      }

      case ObjectType::LIST: {
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

class Parser
{
  private:

    std::vector<alisp::Token*> tokens;
    size_t current_token;

  public:

    Parser(std::vector<alisp::Token*> tokens_) :
        tokens(std::move(tokens_)),
        current_token(0)
    {
        

    }

    void nextToken()
    {
        ++(this->current_token);
    }
    
    alisp::Token* currentToken()
    {
        if (this->current_token >= tokens.size())
            return nullptr;
                     
        return tokens[this->current_token];
    }


    std::vector<Object*> parseWhole()
    {
        std::vector<Object*> objs;
        while (this->current_token < this->tokens.size())
        {
            objs.push_back(parse());
        }
        return objs;
    }

    Object* parse()
    {
        
        const Token* token = currentToken();
        if (token == nullptr)
            return nullptr;
        
        switch (token->getType()) {
            
          case TokenType::LEFT_BRACKET: {
              Object *list = new Object();
              list->type = ObjectType::LIST;
              // std::cout << "New list!" << "\n";

              list->content = std::vector<Object*>();
              auto& obj_list = std::get<std::vector<Object*>>(list->content);

              nextToken();
              while(true)
              {
                  token = currentToken();

                  if(token == nullptr)
                  {
                      std::cout << this->current_token << "\n";
                      std::cout << "Malformed sexp!" << "\n";
                      exit(1);
                  }
                  
                  if(token->getType() == TokenType::RIGHT_BRACKET)
                  {
                      nextToken();
                      // std::cout << "end list" << "\n";
                      break;
                  }
                  
                  Object* next_obj = parse();
                  if(next_obj != nullptr)
                  {
                      obj_list.push_back(next_obj);
                  }                  
                           
              }
              return list;
          }
              
          case TokenType::STRING : {
              //std::cout << "new str" << "\n";
              Object *str = new Object();
              str->type = ObjectType::STRING;
              const STRING_TOKEN* cur = static_cast<const STRING_TOKEN*>(token);
              str->content = cur->getContent();
              nextToken();
              return str;
          }
              
          case TokenType::NUMBER : {
              // std::cout << "new num" << "\n";
              Object *num = new Object();
              num->type = ObjectType::INT;
              const NUMBER_TOKEN* cur = static_cast<const NUMBER_TOKEN*>(token);
              num->content = cur->getContent();
              nextToken();
              return num;
          }

          case TokenType::REAL_NUMBER : {
              // std::cout << "new real" << "\n";
              Object *num = new Object();
              num->type = ObjectType::REAL;
              const REAL_NUMBER_TOKEN* cur = static_cast<const REAL_NUMBER_TOKEN*>(token);
              num->content = cur->getContent();
              nextToken();
              return num;
          }
              
          case TokenType::ID : {
              // std::cout << "new id" << "\n";
              Object *symbol = new Object();
              symbol->type = ObjectType::SYMBOL;
              const ID_TOKEN* cur = static_cast<const ID_TOKEN*>(token);
              symbol->content = cur->getContent();
              nextToken();
              return symbol;
          }
              
          default:
              return nullptr;
        }
        
        return nullptr;
    }

};

}
