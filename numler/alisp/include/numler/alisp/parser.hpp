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
    CELL,
    STRING,
    PROCEDURE,
    PRIMITIVE
};

struct Object
{

    ObjectType type;

    Object():
        content()
    {
    };        
    
    std::variant<int, std::string, std::vector<Object*>> content;
};

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
    

    Object* parse()
    {
        const Token* token = currentToken();
        if (token == nullptr)
            return nullptr;
        
        switch (token->getType()) {
            
          case TokenType::LEFT_BRACKET: {
              Object *list = new Object();
              list->type = ObjectType::LIST;
              std::cout << "New list!" << "\n";

              list->content = std::vector<Object*>();
              auto obj_list = std::get<std::vector<Object*>>(list->content);
              
              while(true)
              {
                  nextToken();
                  token = currentToken();

                  
                  if(token == nullptr)
                  {
                      std::cout << "Malformed sexp!" << "\n";
                      exit(1);
                  }

                  
                  if(token->getType() == TokenType::RIGHT_BRACKET)
                  {
                      nextToken();
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
              std::cout << "New String" << "\n";
              Object *str = new Object();
              str->type = ObjectType::STRING;
              STRING_TOKEN* cur = (STRING_TOKEN*)token;
              str->content = cur->getContent();
              return str;
          }
              
          case TokenType::NUMBER : {
              std::cout << "New Number" << "\n";
              Object *num = new Object();
              num->type = ObjectType::INT;
              NUMBER_TOKEN* cur = (NUMBER_TOKEN*)token;
              num->content = cur->getContent();
              return num;
          }

          case TokenType::ID : {
              std::cout << "New ID" << "\n";
              Object *symbol = new Object();
              symbol->type = ObjectType::SYMBOL;
              ID_TOKEN* cur = (ID_TOKEN*)token;
              symbol->content = cur->getContent();
              return symbol;
          }
              
          default:
              return nullptr;
        }
        
        return nullptr;
    }

};

}
