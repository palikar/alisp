/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#include <iostream>
#include <string>
#include <fstream>
#include <filesystem>
#include <atomic>
#include <functional>
#include <signal.h>
#include <cstring>


#include <clipp.hpp>
#include <fmt/format.h>
#include <rang.hpp>

#include "alisp/config.hpp"
#include "alisp/utility.hpp"
#include "alisp/applications/prompt.hpp"
#include "alisp/alisp/alisp_engine.hpp"

int interactive(alisp::LanguageEngine& alisp_engine);

alisp::LanguageEngine* g_alisp_engine = nullptr;


void got_signal(int c)
{
    signal(SIGINT, got_signal);
    
    if (g_alisp_engine) {

        alisp::prompt::SaveHistory hist;

        try {
            g_alisp_engine->handle_signal(c);
        } catch (...) {
            alisp::handle_errors_lippincott<false>();
        }

    }
}

struct Options
{
    std::string eval{""};
    std::string input{""};

    std::vector<std::string> args;

    std::vector<std::string> includes;

    bool parse_debug{false};
    bool eval_debug{false};
    bool version{false};
    bool interactive{false};
    bool show_help{false};
    bool quick{false};
};

Options opts{};

int main(int argc, char *argv[])
{

    alisp::logging::init_logging();

    

    auto cli = (
        
        opts.version      << clipp::option("-v", "--version")                               % "Show the version and build information of the current executable",
        opts.show_help    << clipp::option("-h", "--help")                                  % "Print help information",
        opts.interactive  << clipp::option("-i", "--interactive")                           % "Start interactive mode after file evaluation",
        opts.parse_debug  << clipp::option("-d", "--parse-debug")                           % "Debug output from the parser",
        opts.eval_debug   << clipp::option("-l", "--eval-debug")                            % "Debug output from the evaluator",
        opts.quick        << clipp::option("-Q", "--quick-start")                           % "Do not loady any scripts on initialization",

        clipp::repeatable( clipp::option("-I") & clipp::integers("include", opts.includes)) % "Extra include directories for module imports.",

        clipp::option("-e", "--eval")         & opts.eval << clipp::value("expr")           % "Input string to evaluate",
        
        
        opts.input         << clipp::opt_value("file")                                      % "Input file" &
        !(opts.args        << clipp::opt_values("args"))                                    % "Arguments for the script being ran."

        );




    auto fmt = clipp::doc_formatting{}
    .first_column(8)                                //left border column for text body
         .doc_column(20)                            //column where parameter docstring starts
         .last_column(80)                           //right border column for text body
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
        return 0;
    }

    if (opts.version) {
        std::cout << alisp::get_build_info();
        return 0;
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

    std::vector<alisp::EngineSettings> settings;

    if (opts.eval_debug) settings.push_back(alisp::EngineSettings::EVAL_DEBUG);
    if (opts.parse_debug) settings.push_back(alisp::EngineSettings::PARSER_DEBUG);
    if (opts.quick) settings.push_back(alisp::EngineSettings::QUICK_INIT);

    alisp::LanguageEngine alisp_engine{settings, std::move(opts.args), std::move(opts.includes)};
    g_alisp_engine = &alisp_engine;

    struct sigaction sa;
    memset( &sa, 0, sizeof(sa) );
    sa.sa_handler = got_signal;
    sigfillset(&sa.sa_mask);
    sigaction(SIGINT,&sa,NULL);


    if(!opts.input.empty()){
        auto file_path = std::filesystem::path{opts.input};
        
        if (!std::filesystem::is_regular_file(file_path)){
            std::cerr << '\"' <<file_path << "\" is not a file." << "\n";
            exit(1);
        }

        alisp_engine.eval_file(file_path);
        if (opts.interactive) { interactive(alisp_engine); }

        exit(0);
    }


    if(!opts.eval.empty()){
        auto [succ, val] = alisp_engine.eval_statement(opts.eval);
        return val;
    }

    return interactive(alisp_engine);
}


int interactive(alisp::LanguageEngine& alisp_engine)
{
    namespace fs = std::filesystem;

    auto alisp_hisotry = fs::path(alisp_engine.get_home()) / PROMPT_HISTORY_FILE;

    if (!fs::is_regular_file(alisp_hisotry)) {
        std::ofstream file { alisp_hisotry };
    }

    alisp::prompt::init(alisp_hisotry);
    
    alisp::prompt::SaveHistory hist;

    std::cout << alisp::get_build_info();

    while(true){
        
        auto command = alisp::prompt::repl(">>> ");

        if(!command) {
            std::cout << '\n';
            break;
        }

        auto [succ, val] = alisp_engine.eval_statement(command.value());

        if (!succ) { return val; }
    }

    return 0;
}
