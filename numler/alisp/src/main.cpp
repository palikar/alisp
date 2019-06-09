#include <iostream>

#include "numler/config.hpp"

#include "numler/alisp/common.hpp"
#include "numler/alisp/lexer.hpp"
#include "numler/alisp/parser.hpp"


int main()
{
  
    alisp::Lexer lex{};
    
    auto toks = lex.tokenize(R"((nil))");


    for (const auto& t : toks)
    {
        std::cout << t->str() << "\n";
    }

    alisp::Parser pars{toks};

    alisp::Object* res = pars.parse();
    std::cout << (int)res->type << "\n";

  
    return 0;

}
