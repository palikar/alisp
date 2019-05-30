#include <iostream>

#include "numler/config.hpp"

#include "numler/alisp/common.hpp"
#include "numler/alisp/lexer.hpp"


int main()
{
  
    alisp::Lexer lex{};

    //   auto toks = lex.tokenize(R"((defun vlad-cc-style()
    // (c-set-style \"linux\")
    // (c-set-offset \'innamespace \'0)
    // (c-set-offset \'inextern-lang \'0)
    // (c-set-offset \'inline-open \'0)
    // (c-set-offset \'label \'*)
    // (c-set-offset \'case-label \'*)
    // (c-set-offset \'access-label '\/)
    // (setq c-basic-offset 4)
    // (setq tab-width 4)
    // (setq indent-tabs-mode nil)))");

    
    auto toks = lex.tokenize(R"((defun vlad-cc-style(   
arg1 &arg2)))");
    
    

    for (const auto& t : toks)
    {
        std::cout << t->str() << "\n";
    }


    // std::cout << alisp::LEFT_BRACE_TOKEN()  << "\n";
    // std::cout << alisp::RIGHT_BRACE_TOKEN()  << "\n";
    // std::cout << alisp::STRING_TOKEN("Some random string")  << "\n";
    // std::cout << alisp::NUMBER_TOKEN(123)  << "\n";
  
    return 0;

}
