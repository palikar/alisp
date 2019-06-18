#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <cmath>

#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/error_messaging.hpp"

namespace alisp
{


class ALLexer
{
  private:
    size_t char_num = 0;
    size_t line_num = 0;

    const ErrorMessanger& err;
    
  public:

    
    explicit ALLexer(const ErrorMessanger& err_);
    
    std::vector<alisp::ALToken> tokenize(const std::string& input);
  
};


}
