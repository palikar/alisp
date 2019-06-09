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
		

#include "numler/alisp/common_lexer.hpp"


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


struct Object;
struct Cell
{
    Object* con;
    Object* cdr;
};

struct Object
{
    ObjectType type;
    std::variant<int, float, std::string, std::vector<Object*>> content;

    Object(ObjectType type_):type(type_),content(){};
    Object():content(){};
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

    std::vector<alisp::Token> tokens;
    size_t current_token;

  public:

    Parser(const std::vector<alisp::Token>& tokens_) :
        tokens(tokens_),
        current_token(0)
    {
        

    }

    void nextToken()
    {
        ++(this->current_token);
    }
    
    std::optional<Token> currentToken()
    {
        if (this->current_token >= tokens.size())
        {
            return std::nullopt;
        }
        return this->tokens[this->current_token];
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
        
        auto tok = currentToken();
        if (!tok)
            return nullptr;

        Token& token = tok.value();
        
        switch (token.getType()) {
            
          case TokenType::LEFT_BRACKET: {
              Object *list = new Object(ObjectType::LIST);
              
              list->content = std::vector<Object*>();
              auto& obj_list = std::get<std::vector<Object*>>(list->content);

              nextToken();
              while(true)
              {
                  tok = currentToken();
                  if(!tok)
                  {
                      std::cout << "Malformed sexp!" << "\n";
                      exit(1);
                  }

                  token = tok.value();
                  
                  if(token.getType() == TokenType::RIGHT_BRACKET)
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
              Object *str = new Object(ObjectType::STRING);
              str->content = token.getContentAs<std::string>();
              nextToken();
              return str;
          }
              
          case TokenType::NUMBER : {
              Object *num = new Object(ObjectType::INT);
              num->content = token.getContentAs<int>();
              nextToken();
              return num;
          }

          case TokenType::REAL_NUMBER : {
              Object *num = new Object(ObjectType::REAL);
              num->content = token.getContentAs<float>();
              nextToken();
              return num;
          }
              
          case TokenType::ID : {
              Object *symbol = new Object(ObjectType::SYMBOL);
              symbol->content = token.getContentAs<std::string>();
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
