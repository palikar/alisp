#include <iostream>

#include "alisp/config.hpp"
#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"
#include "alisp/alisp/parser.hpp"
#include "alisp/alisp/error_messaging.hpp"

int main(int, char *argv[])
{

    
    

    alisp::ErrorMessanger err;
    alisp::ALLexer lex{err};


    std::string input{R"(
 (defun (a b c &optional)
    "this is sick"
    (setq a b)
    (if a (setq b 3) (setq c 4))
    (progn
       (message "Hello World!")
       (if a (setq b 3) (setq c 4))))

)"};


    err.set_input(input);
    auto toks = lex.tokenize(input);


    // err.set_input(argv[1]);
    // auto toks = lex.tokenize(argv[1]);

    
    alisp::ALParser pars{toks};    
    auto res = pars.parseWhole();
    for (auto r : res) {
        std::cout << ":";
        alisp::printObject(r);
        std::cout << "\n";
    }


  
    return 0;

}
