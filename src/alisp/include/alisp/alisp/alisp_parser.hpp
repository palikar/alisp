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

    void next_token()
    {
        ++(this->current_token_pos);
    }

    std::optional<ALToken> current_token()
    {
        if (this->current_token_pos >= tokens.size())
        {
            return std::nullopt;
        }
        return this->tokens[this->current_token_pos];
    }

    ALObject* parse_internal()
    {
        auto tok = current_token();
        if (!tok)
            return nullptr;

        ALToken& token = tok.value();

        switch (token.getType()) {

          case TokenType::LEFT_BRACKET: {
              next_token();
              return parse_list();
          }

          case TokenType::STRING : {
              return parse_string();
          }

          case TokenType::NUMBER : {
              return parse_number();
          }

          case TokenType::REAL_NUMBER : {
              return parse_real();
          }

          case TokenType::ID : {
              return parse_id();
          }

          case TokenType::COLON : {
              return parse_pword();
          }

          case TokenType::KEYWORD : {
              return parse_id();
          }

              // case TokenType::AT : {
              //     return parse_backquote_splice();
              // }

          case TokenType::COMMA : {
              return parse_backquote_eval();
          }

          case TokenType::HASHTAG : {
              return parse_function();
          }
              
          case TokenType::RIGHT_BRACKET : {
              std::cout << "Unexpected right bracket" << "\n";
              exit(1);
          }

          case TokenType::QUOTE : {
              return parse_quote();
          }

          case TokenType::BACKQUOTE : {
              return parse_backquote();
          }

          default:
              return nullptr;
        }

        return nullptr;

    }

    ALObject* parse_backquote()
    {
        
        //check if quote
        next_token();
        ALObject *cell = new ALObject(ALObjectType::LIST);
        cell->content = std::vector<ALObject*>();
        auto& list = std::get<std::vector<ALObject*>>(cell->content);

        ALObject *quote = new ALObject(ALObjectType::ID);
        quote->content = std::string{"semi-quote"};
        list.push_back(quote);


        ALObject *obj = parse_internal();
        list.push_back(obj);

        return cell;

    
    }
    
    ALObject* parse_backquote_eval()
    {
        //check if comma
        next_token();
        if(current_token().value().getType() == TokenType::AT)
        {
            next_token();
            return parse_backquote_splice();
        }
        
        ALObject *cell = new ALObject(ALObjectType::LIST);
        cell->content = std::vector<ALObject*>();
        auto& list = std::get<std::vector<ALObject*>>(cell->content);

        ALObject *quote = new ALObject(ALObjectType::ID);
        quote->content = std::string{"semi-quote-eval"};
        list.push_back(quote);


        ALObject *obj = parse_internal();
        list.push_back(obj);

        return cell;
    }

    ALObject* parse_backquote_splice()
    {
        ALObject *cell = new ALObject(ALObjectType::LIST);
        cell->content = std::vector<ALObject*>();
        auto& list = std::get<std::vector<ALObject*>>(cell->content);

        ALObject *quote = new ALObject(ALObjectType::ID);
        quote->content = std::string{"semi-quote-splice"};
        list.push_back(quote);

        ALObject *obj = parse_internal();
        list.push_back(obj);

        return cell;        
    }

    ALObject* parse_function()
    {
        
        //check if hashtag
        next_token();
        //check if quote
        next_token();
        ALObject *cell = new ALObject(ALObjectType::LIST);
        cell->content = std::vector<ALObject*>();
        auto& list = std::get<std::vector<ALObject*>>(cell->content);

        ALObject *quote = new ALObject(ALObjectType::ID);
        quote->content = std::string{"function"};
        list.push_back(quote);


        ALObject *obj = parse_internal();
        list.push_back(obj);

        return cell;
    }
    
    ALObject* parse_pword()
    {
        // TODO: check if colon
        next_token();
        // TODO: check if id
        auto token = current_token().value();
        ALObject *id = new ALObject(ALObjectType::PWORD);
        id->content = ":" + token.getString();
        next_token();
        return id;
    }

    ALObject* parse_arg_list()
    {
        //check opening bracket
        //error if opening bracket
        //check clsoing bracket
        return nullptr;
    }

    ALObject* parse_list()
    {
        ALObject *list = new ALObject(ALObjectType::LIST);
        list->content = std::vector<ALObject*>();
        auto& obj_list = std::get<std::vector<ALObject*>>(list->content);

        // if (peek().getString().equals("defun"))
        // {
        //     ALObject *def = new ALObject(ALObjectType::ID);
        //     def->content = std::string{"defun"};
        //     obj_list.push_back(def);

        //     next_token();
            
        //     ALObject *arg_list = parse_arg_list();
        //     obj_list.push_back(arg_list);
            
        // }
        
        while(true)
        {
            auto tok_opt = current_token();
            if(!tok_opt)
            {
                std::cout << "Malformed sexp!" << "\n";
                exit(1);
            }

            auto token = tok_opt.value();

            
            if(token.getType() == TokenType::RIGHT_BRACKET)
            {
                next_token();
                break;
            }

            if(token.getType() == TokenType::RIGHT_BRACKET)
            {
                next_token();
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

    ALObject* parse_quote()
    {
        //check if quote
        next_token();
        ALObject *cell = new ALObject(ALObjectType::LIST);
        cell->content = std::vector<ALObject*>();
        auto& list = std::get<std::vector<ALObject*>>(cell->content);

        ALObject *quote = new ALObject(ALObjectType::ID);
        quote->content = std::string{"quote"};
        list.push_back(quote);


        ALObject *obj = parse_internal();
        list.push_back(obj);

        return cell;

    }

    ALObject* parse_id()
    {
        auto token = current_token().value();
        ALObject *id = new ALObject(ALObjectType::ID);
        id->content = token.getString();
        next_token();
        return id;
    }

    ALObject* parse_string()
    {
        auto token = current_token().value();
        ALObject *str = new ALObject(ALObjectType::STRING);
        str->content = token.getString();
        next_token();
        return str;

    }

    ALObject* parse_number()
    {
        auto token = current_token().value();
        ALObject *num = new ALObject(ALObjectType::INT);
        num->content = token.getNumber();
        next_token();
        return num;

    }

    ALObject* parse_real()
    {
        auto token = current_token().value();
        ALObject *num = new ALObject(ALObjectType::REAL);
        num->content = token.getReal();
        next_token();
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
