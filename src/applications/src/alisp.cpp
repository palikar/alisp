#include <iostream>
#include <string>
#include <fstream>
#include <filesystem>

#include <clara.hpp>
#include <fmt/format.h>
#include <rang.hpp>

#include "alisp/config.hpp"
#include "alisp/utility.hpp"

#include "alisp/applications/prompt.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"







void eval_statement(const std::string& command);
void interactive();
void eval_file(const std::filesystem::path& t_path);

bool env_bool(const char* t_name)
{
    return getenv(t_name) != nullptr;
}

std::string env_string(const char* t_name)
{
    if (env_bool(t_name)){
        return std::string{getenv(t_name)};
    }
    return {};
}



struct Options
{
    std::string eval{""};
    std::string input{""};
    bool parse_debug;
    bool eval_debug;
    bool version;
    bool interactive;
};

Options opts{};


int main(int argc, char *argv[])
{

    

    using clara::Opt;
    using clara::Arg;
    using clara::Args;
    using clara::Help;

    bool showHelp{ false };


    auto cli = Help(showHelp)
        | Opt(opts.version)["-v"]["--version"]("Show the version of alisp")
        | Opt(opts.eval, "string")["-e"]["--eval"]("Input to evaluate")
        | Opt(opts.parse_debug)["-d"]["--parse-debug"]("Debug output from the parser")
        | Opt(opts.eval_debug)["-l"]["--eval-debug"]("Debug output from the evaluator")
        | Opt(opts.interactive)["-i"]["--iteractive"]("Start interactive mode after file evaluation")
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
        exit(0);
    }

    if(!opts.input.empty()){
        auto file_path = std::filesystem::path{opts.input};
        if (!std::filesystem::is_regular_file(file_path)){
            exit(1);
        }

        eval_file(file_path);
        if (opts.interactive) interactive();
        exit(0);
    }


    if(!opts.eval.empty()){
        eval_statement(opts.eval);
        exit(0);;
    }

    interactive();

    return 0;
}



alisp::env::Environment env;
alisp::eval::Evaluator eval(env);
alisp::parser::ALParser<alisp::env::Environment> pars(env);

void interactive()
{

    std::cout << alisp::get_build_info();
    alisp::prompt::init();

    while(true){
        auto command = alisp::prompt::repl(">>> ");
        eval_statement(command);

    }
}


void eval_statement(const std::string& command)
{
    try {
        auto parse_res = pars.parse(&command, "__EVAL__");

        for (auto p : parse_res ) {
            if (opts.parse_debug) std::cout << "DEUBG[PARSER]: " << alisp::dump(p) << "\n";

            auto eval_res = eval.eval(p);
            if (opts.eval_debug) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_res) << "\n";
            std::cout << *eval_res << "\n";

        }

    } catch (alisp::parse_exception& p_exc) {
        std::cout << rang::fg::red << "Parser error:\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
        exit(1);
    } catch (alisp::environment_error& p_exc) {
        std::cout << rang::fg::red << "Environment error:\n" << rang::fg::reset;
        std::cout << p_exc.what() << "\n";
        exit(1);
    }



}


void eval_file(const std::filesystem::path& t_path)
{


    if (std::ifstream ifs{ t_path, std::ios::binary }; ifs.good()) {
        const auto file_size = ifs.seekg(0, std::ios_base::end).tellg();
        ifs.seekg(0);
        std::vector<char> data;
        data.resize(static_cast<std::size_t>(file_size));
        ifs.read(data.data(), file_size);


        std::string command{ begin(data), end(data) };
        try {
            auto parse_res = pars.parse(&command, std::filesystem::absolute(t_path));

            for (auto p : parse_res ) {

                if (opts.parse_debug) std::cout << "DEUBG[PARSER]: " << alisp::dump(p) << "\n";

                auto eval_res = eval.eval(p);
                if (opts.eval_debug) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_res) << "\n";

            }

        } catch (alisp::parse_exception& p_exc) {
            std::cout << rang::fg::red << "Parser error:\n" << rang::fg::reset;
            std::cout << p_exc.what() << "\n";
            exit(1);
        } catch (alisp::environment_error& p_exc) {
            std::cout << rang::fg::red << "Environment error:\n" << rang::fg::reset;
            std::cout << p_exc.what() << "\n";
            exit(1);
        }


    }


}
