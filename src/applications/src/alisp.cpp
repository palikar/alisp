#include <iostream>
#include <string>
#include <fstream>
#include <filesystem>

#include <clipp.hpp>
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

    std::vector<std::string> args;

    bool parse_debug{false};
    bool eval_debug{false};
    bool version{false};
    bool interactive{false};
    bool show_help{false};
};

Options opts{};


int main(int argc, char *argv[])
{

    alisp::logging::init_logging();

    auto cli = (
        
        opts.version      << clipp::option("-v", "--version")                             % "Show the version and build information of the current executable",
        opts.show_help    << clipp::option("-h", "--help")                                % "Print help information",
        opts.interactive  << clipp::option("-i", "--interactive")                         % "Start interactive mode after file evaluation",
        opts.eval         << clipp::option("-e", "--eval") & clipp::value("expr")         % "Input string to evaluate",
        opts.parse_debug  << clipp::option("-d", "--parse-debug")                         % "Debug output from the parser",
        opts.eval_debug   << clipp::option("-l", "--eval-debug")                          % "Debug output from the evaluator",

        (opts.input       << clipp::opt_value("file")      % "Input file") &
        (opts.args        << clipp::opt_values("args")     % "Args")

        );


    
    
    auto fmt = clipp::doc_formatting{}
    .first_column(8)                           //left border column for text body
         .doc_column(20)                            //column where parameter docstring starts
         .last_column(80)                          //right border column for text body
         .indent_size(2)                            //indent of documentation lines for children of a documented group
         .line_spacing(0)                           //number of empty lines after single documentation lines
         .paragraph_spacing(1)                      //number of empty lines before and after paragraphs
         .flag_separator(", ")                      //between flags of the same parameter
         .param_separator(" ")                      //between parameters 
         .group_separator(" ")                      //between groups (in usage)
         .alternative_param_separator("|")          //between alternative flags 
         .alternative_group_separator(" | ")        //between alternative groups 
         .surround_group("(", ")")                  //surround groups with these 
         .surround_alternatives("(", ")")           //surround group of alternatives with these
         .surround_alternative_flags("", "")        //surround alternative flags with these
         .surround_joinable("(", ")")               //surround group of joinable flags with these
         .surround_optional("[", "]")               //surround optional parameters with these
         .surround_repeat("", "...")                //surround repeatable parameters with these
         .empty_label("")                           //used if parameter has no flags and no label
         .max_flags_per_param_in_usage(1)           //max. # of flags per parameter in usage
         .max_flags_per_param_in_doc(32)            //max. # of flags per parameter in detailed documentation
         .split_alternatives(true)                  //split usage into several lines for large alternatives
         .alternatives_min_split_size(3)            //min. # of parameters for separate usage line
         .merge_alternative_flags_with_common_prefix(false)  //-ab(cdxy|xy) instead of -abcdxy|-abxy
         .ignore_newline_chars(false);

    // std::cout << "Usage:\n" << clipp::usage_lines(cli, "progname", fmt)
    //           << "\nOptions:\n" << clipp::documentation(cli, fmt) << '\n';

    // std::cout << clipp::make_man_page(cli, "progname", fmt)
    //     .prepend_section("DESCRIPTION", "The alisp programming language.")
    //     .append_section("LICENSE", "GPLv3");

    clipp::parse(argc, argv, cli);    //excludes argv[0]

    
    
    


// const auto result = cli.parse(Args(argc, argv));

    // if (!result) {
    //     std::cerr << "Error in command line: " << result.errorMessage() << '\n';
    // }

    // if (showHelp) {
    //     std::cout << cli << '\n';
    //     exit(1);
    // }

    // if (opts.version) {

    //     std::cout << fmt::format("ALisp {}.{}.{}", alisp::version_major, alisp::version_minor, alisp::version_patch) << '\n';
    //     exit(0);
    // }

    // if(!opts.input.empty()){
    //     auto file_path = std::filesystem::path{opts.input};
    //     if (!std::filesystem::is_regular_file(file_path)){
    //         exit(1);
    //     }

    //     eval_file(file_path);
    //     if (opts.interactive) interactive();
    //     exit(0);
    // }


    // if(!opts.eval.empty()){
    //     eval_statement(opts.eval);
    //     exit(0);;
    // }

    // interactive();

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
