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



#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"


namespace alisp
{

namespace parser
{



template <class ErrorHandler>
class ALParser
{
  private:

    std::vector<alisp::ALToken> tokens;
    size_t current_token_pos;

    const ErrorHandler &err;

  public:

    ALParser(const ErrorHandler &err_)
        : current_token_pos(0), err(err_)
    {
    }

  private:

    ALToken peek()
    {
        return this->tokens[this->current_token_pos+1];
    }

    ALToken peek(size_t distance)
    {
        return this->tokens[this->current_token_pos + distance];
    }

    void nextToken()
    {
        ++(this->current_token_pos);
    }

    std::optional<ALToken> currentToken()
    {
        if (this->current_token_pos >= tokens.size())
        {
            return std::nullopt;
        }
        return this->tokens[this->current_token_pos];
    }

    ALObject* parse_internal()
    {
        auto tok = currentToken();
        if (!tok)
            return nullptr;

        ALToken& token = tok.value();

        switch (token.getType()) {

          case TokenType::LEFT_BRACKET: {
              nextToken();
              return parseList();
          }

          case TokenType::STRING : {
              return parseString();
          }

          case TokenType::NUMBER : {
              return parseNumber();
          }

          case TokenType::REAL_NUMBER : {
              return parseReal();
          }

          case TokenType::ID : {
              return parseId();
          }

          case TokenType::KEYWORD : {
              return parseId();
          }

          case TokenType::RIGHT_BRACKET : {
              std::cout << "Unexpected right bracket" << "\n";
              exit(1);
          }

          case TokenType::QUOTE : {
              return parseQuote();
          }

          default:
              return nullptr;
        }

        return nullptr;

    }

    ALObject* parse_arg_list()
    {
        //check opening bracket
        //error if opening bracket
        //check clsoing bracket
        return nullptr;
    }

    ALObject* parseList()
    {
        ALObject *list = new ALObject(ALObjectType::LIST);
        list->content = std::vector<ALObject*>();
        auto& obj_list = std::get<std::vector<ALObject*>>(list->content);

        // if (peek().getString().equals("defun"))
        // {
        //     ALObject *def = new ALObject(ALObjectType::ID);
        //     def->content = std::string{"defun"};
        //     obj_list.push_back(def);

        //     nextToken();
            
        //     ALObject *arg_list = parse_arg_list();
        //     obj_list.push_back(arg_list);
            
        // }
        
        while(true)
        {
            auto tok_opt = currentToken();
            if(!tok_opt)
            {
                std::cout << "Malformed sexp!" << "\n";
                exit(1);
            }

            auto token = tok_opt.value();

            
            if(token.getType() == TokenType::RIGHT_BRACKET)
            {
                nextToken();
                break;
            }

            if(token.getType() == TokenType::RIGHT_BRACKET)
            {
                nextToken();
                break;
            }

            ALObject* next_obj = parse_internal();
            if(next_obj != nullptr)
            {
                obj_list.push_back(next_obj);
            }

        }
        return list;
    }

    ALObject* parseQuote()
    {
        ALObject *cell = new ALObject(ALObjectType::LIST);
        cell->content = std::vector<ALObject*>();
        auto& list = std::get<std::vector<ALObject*>>(cell->content);

        ALObject *quote = new ALObject(ALObjectType::ID);
        quote->content = std::string{"quote"};
        list.push_back(quote);

        nextToken();

        ALObject *obj = parse_internal();
        list.push_back(obj);

        return cell;

    }

    ALObject* parseId()
    {
        auto token = currentToken().value();
        ALObject *id = new ALObject(ALObjectType::ID);
        id->content = token.getString();
        nextToken();
        return id;
    }

    ALObject* parseString()
    {
        auto token = currentToken().value();
        ALObject *str = new ALObject(ALObjectType::STRING);
        str->content = token.getString();
        nextToken();
        return str;

    }

    ALObject* parseNumber()
    {
        auto token = currentToken().value();
        ALObject *num = new ALObject(ALObjectType::INT);
        num->content = token.getNumber();
        nextToken();
        return num;

    }

    ALObject* parseReal()
    {
        auto token = currentToken().value();
        ALObject *num = new ALObject(ALObjectType::REAL);
        num->content = token.getReal();
        nextToken();
        return num;

    }

  public:

    std::vector<ALObject*> parse(const std::vector<alisp::ALToken>& tokens_)
    {
        this->tokens = tokens_;
        std::vector<ALObject*> objs;
        while (this->current_token_pos < this->tokens.size())
        {
            objs.push_back(parse_internal());
        }
        return objs;
    }


};


}

}
