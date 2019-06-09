#include <iostream>

#include "numler/config.hpp"

#include "numler/alisp/common.hpp"
#include "numler/alisp/lexer.hpp"
#include "numler/alisp/parser.hpp"


int main()
{
  
    alisp::Lexer lex{};
    
    auto toks = lex.tokenize(R"((if 2 (progn (setq a 2) (setq a 2)))");
    // auto toks = lex.tokenize(R"((if "asdsa" -2 (setq a) (setq b 3)))");


    for (const auto& t : toks)
    {
        std::cout << t->str() << "\n";
    }

    alisp::Parser pars{toks};
    auto res = pars.parseWhole();
    for (auto r : res) {
        std::cout << "1:";
        alisp::printObject(r);
        std::cout << "\n";
    }


  
    return 0;

}
