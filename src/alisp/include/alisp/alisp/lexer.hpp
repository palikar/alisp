#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <cmath>

#include "alisp/alisp/common_lexer.hpp"


namespace alisp
{


class ALLexer
{
  private:
    int char_num = 0;
    int line_num = 0;
    
  public:
    ALLexer();
    std::vector<alisp::Token> tokenize(const std::string& input);

  
};


}
