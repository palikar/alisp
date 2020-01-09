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
#include "alisp/alisp/alisp_engine.hpp"



void eval_statement(std::string& command);
void interactive(alisp::LanguageEngine& alisp_engine);
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

    auto res = clipp::parse(argc, argv, cli);

    if(res.any_bad_repeat() or res.any_blocked() or res.any_conflict()) {
        std::cout << "Usage:\n" << clipp::usage_lines(cli, "progname", fmt) << "\n";
        exit(1);
    }    
    

    if (opts.show_help) {
        std::cout << clipp::make_man_page(cli, "progname", fmt)
            .prepend_section("DESCRIPTION", "The alisp programming language.")
            .append_section("LICENSE", "GPLv3");
        exit(0);
    }

    if (opts.version) {
        std::cout << alisp::get_build_info();
        exit(0);
    }


    // std::cout << "File:" << opts.input << "\n";
    // std::cout << "Eval:" << opts.eval << "\n";
    // std::cout << "Parse debug:" << opts.parse_debug << "\n";
    // std::cout << "Eval debug:" << opts.eval_debug << "\n";
    // std::cout << "Interactive:" << opts.interactive << "\n";
    // std::cout << "Help:" << opts.show_help << "\n";
    // std::cout << "Version:" << opts.version << "\n";
    // for (auto& it : opts.args) {
    //     std::cout << "arg: "  << it << "\n";
    // }

    alisp::LanguageEngine alisp_engine;

    
    if(!opts.input.empty()){
        auto file_path = std::filesystem::path{opts.input};
        if (!std::filesystem::is_regular_file(file_path)){
            std::cerr << '\"' <<file_path << "\" is not a file." << "\n";
            exit(1);
        }
        
        alisp_engine.eval_file(file_path);
        if (opts.interactive) { interactive(); }

        exit(0);
    }


    if(!opts.eval.empty()){
        alisp_engine.eval_statement(opts.eval);
        exit(0);;
    }

    interactive(alisp_engine);

    return 0;
}


void interactive(alisp::LanguageEngine& alisp_engine)
{

    std::cout << alisp::get_build_info();
    alisp::prompt::init();

    while(true){
        auto command = alisp::prompt::repl(">>> ");
        alisp_engine.eval_statement(command);

    }
}
