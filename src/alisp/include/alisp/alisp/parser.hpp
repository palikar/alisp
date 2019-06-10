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



#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lisp_object.hpp"


namespace alisp
{



class ALParser
{
  private:

    std::vector<alisp::Token> tokens;
    size_t current_token;

  public:

    ALParser(const std::vector<alisp::Token>& tokens_);
    
    Token peek();
    void nextToken();
    std::optional<Token> currentToken();


    std::vector<Object*> parseWhole();
    Object* parse();


};

}
