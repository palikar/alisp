#include <iostream>
#include <string>
#include <clara.hpp>


#include "alisp/config.hpp"
#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"
#include "alisp/alisp/parser.hpp"
#include "alisp/alisp/error_messaging.hpp"

#include "alisp/alisp/prompt.hpp"


void eval_statement(const std::string& command);


struct Options
{
    std::string eval{""};
    std::string input{""};
    bool version;
};

int main(int argc, char *argv[])
{
    using clara::Opt;
    using clara::Arg;
    using clara::Args;
    using clara::Help;

    bool showHelp{ false };
    Options opts{};

    auto cli = Help(showHelp)
        | Opt(opts.version)["-v"]["--version"]("Show the version of alisp")
        | Opt(opts.eval, "string")["-e"]["--eval"]("Input to evaluate")
        | Arg(opts.input, "file")("Input file");
                

    const auto result = cli.parse(Args(argc, argv));

    if (!result) {
        std::cerr << "Error in command line: " << result.errorMessage() << '\n';
    }
            
    if (showHelp) {
        std::cout << cli << '\n';
        exit(1);
    }

    if (opts.version) {
        std::cout << "Alisp 0.0.0" << '\n';
        exit(1);
    }

    if(!opts.input.empty()){
        std::cout << "input" << "\n";        
        return 0;
    }

    if(!opts.eval.empty()){
        eval_statement(opts.eval);
        return 0;
    }


    alisp::prompt::init();
    alisp::prompt::repl(">>> ");
    
    
    return 0;
}



void eval_statement(const std::string& command)
{

    alisp::ErrorMessanger err;
    alisp::ALLexer lex{err};
    err.set_input(command);
    
    auto toks = lex.tokenize(command);
    alisp::ALParser pars{toks};

    for (const auto& t : toks)
    {
        std::cout << t << "\n";
    }
    
    // auto res = pars.parseWhole();

    // for (auto r : res) {
    //     std::cout << ":";
    //     alisp::printObject(r);
    //     std::cout << "\n";
    // }

}
