#include <iostream>
#include <string>
#include <clara.hpp>
#include <fmt/format.h>

#include "alisp/config.hpp"
#include "alisp/utility/defines.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_lexer.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/error_messaging.hpp"
#include "alisp/applications/prompt.hpp"

#include "alisp/utility.hpp"



void eval_statement(const std::string& command);


struct Options
{
    std::string eval{""};
    std::string input{""};
    bool version;
};

int main(int argc, char *argv[])
{

    alisp::logging::init_logging();
        
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
        
        std::cout << fmt::format("ALisp {}.{}.{}", alisp::version_major, alisp::version_minor, alisp::version_patch) << '\n';
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

    std::cout << alisp::get_build_info();
    alisp::prompt::init();
    alisp::prompt::repl(">>> ");
    
    
    return 0;
}



void eval_statement(const std::string& command)
{

    alisp::ErrorMessanger err;
    alisp::parser::ALParser pars{err};

    err.set_input(command);

    auto res = pars.parse(command, "--eval--");

    for (auto r : res) {
        std::cout << ":";
        alisp::util::printObject(r);
        std::cout << "\n";
    }

}
