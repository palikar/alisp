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



class ALParser
{
  private:

    std::vector<alisp::ALToken> tokens;
    size_t current_token;

  public:

    ALParser(const std::vector<alisp::ALToken>& tokens_);
    
    ALToken peek();
    void nextToken();
    std::optional<ALToken> currentToken();


    std::vector<ALObject*> parseWhole();
    ALObject* parse();


};

}
