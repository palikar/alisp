#pragma once

#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>
#include <bitset>
#include <cstdint>
#include <utility>
#include <memory>
#include <filesystem>
#include <fstream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_parser.hpp"

#include "alisp/utility/files.hpp"


namespace alisp
{

namespace detail
{


}

enum class EngineSettings
{
    PARSER_DEBUG,
    EVAL_DEBUG
};


/* Tasks for the Engine
   - initilize the system with system specific things;
   - handle environment variables and notify the other components
   - handle the command line arguments for the evaluation
   - find out the right modules paths and give them to the env
   - handle errors at top level
*/

class LanguageEngine
{
  private:

    env::Environment m_environment;
    std::shared_ptr<parser::ALParser<env::Environment>> m_parser;
    eval::Evaluator m_evaluator;

    std::vector<EngineSettings> m_settings;

    bool check(EngineSettings t_setting)
    {
        return std::find(std::begin(m_settings), std::end(m_settings), t_setting) != std::end(m_settings);
    }

    void do_eval(std::string& t_input, const std::string& t_file, bool t_print_res = false)
    {
        auto parse_result = m_parser->parse(t_input, t_file);

        for (auto sexp : parse_result ) {
            if (check(EngineSettings::PARSER_DEBUG)) std::cout << "DEUBG[PARSER]: " << alisp::dump(sexp) << "\n";
            auto eval_result = m_evaluator.eval(sexp);
            if (check(EngineSettings::EVAL_DEBUG)) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_result) << "\n";
            if (t_print_res) { std::cout << *eval_result << "\n"; }
        }
    }


  public:

    static bool env_bool(const char* t_name)
    {
        return getenv(t_name) != nullptr;
    }

    static std::string env_string(const char* t_name)
    {
        if (env_bool(t_name)){
            return std::string{getenv(t_name)};
        }
        return {};
    }



    LanguageEngine(std::vector<EngineSettings> t_setting = {}) :
        m_environment(), m_parser(std::make_shared<parser::ALParser<env::Environment>>(m_environment)),
        m_evaluator(m_environment, m_parser),
        m_settings(t_setting)
    {
        init_system();
    }

    void init_system()
    {

    }


    void eval_statement(std::string& command)
    {
        try {
            do_eval(command, "__EVAL__", true);
        }catch (...) {
            handle_errors_lippincott<false>();
        }

    }


    void eval_file(const std::filesystem::path& t_path)
    {
        try {
            auto file_content = utility::load_file(t_path);
            do_eval(file_content, t_path);
        }catch (...) {
            handle_errors_lippincott<true>();
        }
    }


};


}
