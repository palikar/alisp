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
#include "alisp/alisp/alisp_modules.hpp"

#include "alisp/utility/files.hpp"


namespace alisp
{

static constexpr std::string ENV_VAR_MODPATHS = "ALPATH"

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
    std::vector<std::string> m_argv;
    std::pvector<std::string> m_imports;

    bool check(EngineSettings t_setting) { return std::find(std::begin(m_settings), std::end(m_settings), t_setting) != std::end(m_settings); }

    void do_eval(std::string &t_input, const std::string &t_file, bool t_print_res = false)
    {
        auto parse_result = m_parser->parse(t_input, t_file);

        for (auto sexp : parse_result)
        {
            if (check(EngineSettings::PARSER_DEBUG)) std::cout << "DEUBG[PARSER]: " << alisp::dump(sexp) << "\n";
            auto eval_result = m_evaluator.eval(sexp);
            if (check(EngineSettings::EVAL_DEBUG)) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_result) << "\n";
            if (t_print_res) { std::cout << *eval_result << "\n"; }
        }
    }


  public:
    
    static bool env_bool(const char *t_name) { return getenv(t_name) != nullptr; }

    static std::string env_string(const char *t_name)
    {
        if (env_bool(t_name)) { return std::string{ getenv(t_name) }; }
        return {};
    }


    LanguageEngine(std::vector<EngineSettings> t_setting = {}, std::vector<std::string> t_cla = {},
                   std::vector<std::string> t_extra_imports = {}
        )
        : m_environment()
        , m_parser(std::make_shared<parser::ALParser<env::Environment>>(m_environment))
        , m_evaluator(m_environment, m_parser)
        , m_settings(std::move(t_setting))
        , m_argv(std::move(t_cla))
        , m_imports(std::move(t_extra_imports))
    {
        init_system();
    }

    void init_system()
    {
        env::init_modules();
        logging::init_logging();
        
        env::update_prime(Qcommand_line_args, make_list(m_argv));

        std::string al_path = env_string(ENV_VAR_MODPATHS);
        const auto add_modules = [&](auto &path) { Vmodpaths->children().push_back(make_string(ENV_VAR_MODPATHS)); };
        if (!al_path.empty())
        {
            auto paths = utility::split(al_path, ':');
            std::for_each(std::begin(paths), std::end(paths), add_modules);
        }
        std::for_each(std::begin(m_imports), std::end(m_imports), add_modules);
        
    }


    void eval_statement(std::string &command)
    {
        try
        {
            do_eval(command, "__EVAL__", true);
        }
        catch (...)
        {
            handle_errors_lippincott<false>();
        }
    }

    
    void eval_file(const std::filesystem::path &t_path)
    {

        namespace fs = std::filesystem;
        Vmodpaths->children().push_back(make_string(fs::absolute(t_path.parent_path()))); 
        
        try
        {
            auto file_content = utility::load_file(t_path);
            do_eval(file_content, t_path);
        }
        catch (...)
        {
            handle_errors_lippincott<true>();
        }
    }


    ALObjectPtr get_value(const std::string& t_sym_name)
    {
        return m_environment.find(t_sym_name);
    }


    
};


}  // namespace alisp
