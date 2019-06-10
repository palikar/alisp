#include <iostream>

#include "alisp/config.hpp"
#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"
#include "alisp/alisp/parser.hpp"


int main()
{
  
    alisp::ALLexer lex{};
    
    
    // auto toks = lex.tokenize(R"( (if 2 (progn (setq a 2) (setq a 2))) (if "asdsa" -2 (setq a) (setq b 3)))");
    auto toks = lex.tokenize(R"(
(defun (a b c &optional)
"this is sick"
(setq a b)
(if a (setq b 3) (setq c 4))))");


    for (const auto& t : toks)
    {
        std::cout << t.str() << "\n";
    }

    alisp::ALParser pars{toks};    
    auto res = pars.parseWhole();
    for (auto r : res) {
        std::cout << ":";
        alisp::printObject(r);
        std::cout << "\n";
    }


  
    return 0;

}
