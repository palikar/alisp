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
#include "alisp/alisp/alisp_streams.hpp"
#include "alisp/alisp/alisp_warnings.hpp"
#include "alisp/alisp/alisp_optimizer.hpp"

#include "alisp/utility/files.hpp"
#include "alisp/utility/env.hpp"


namespace alisp
{
inline constexpr auto prelude_directory = AL_PRELUDE_DIR;


enum class EngineSettings
{
    PARSER_DEBUG,
    EVAL_DEBUG,
    QUICK_INIT,
    DISABLE_DEBUG_MODE,
    OPTIMIZATION
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
    std::unique_ptr<parser::ALParser<env::Environment>> m_parser;
    eval::Evaluator m_evaluator;
    optimizer::MainOptimizer g_optimizer;

    std::vector<EngineSettings> m_settings;
    std::vector<std::string> m_argv;
    std::vector<std::string> m_imports;
    std::vector<std::string> m_warnings;

    const std::string m_home_directory;

    bool check(EngineSettings t_setting)
    {
        return std::find(std::begin(m_settings), std::end(m_settings), t_setting) != std::end(m_settings);
    }

    void do_eval(std::string &t_input, const std::string &t_file, bool t_print_res = false);


  public:
    LanguageEngine(std::vector<EngineSettings> t_setting    = {},
                   std::vector<std::string> t_cla           = {},
                   std::vector<std::string> t_extra_imports = {},
                   std::vector<std::string> t_warnings      = {})
      : m_environment()
      , m_parser(std::make_unique<parser::ALParser<env::Environment>>(m_environment))
      , m_evaluator(m_environment, m_parser.get())
      , m_settings(std::move(t_setting))
      , m_argv(std::move(t_cla))
      , m_imports(std::move(t_extra_imports))
      , m_warnings(std::move(t_warnings))
      , m_home_directory(utility::env_string("HOME"))
    {
        init_system();
    }

    ~LanguageEngine() {}

    void init_system();

    void load_init_scripts();

    std::pair<bool, int> eval_statement(std::string &command, bool exit_on_error = true);

    std::pair<bool, int> eval_file(const std::filesystem::path &t_path, bool insert_mod_path = true);

    std::pair<bool, int> eval_objs(std::vector<ALObjectPtr> t_objs);

    ALObjectPtr get_value(const std::string &t_sym_name) { return m_environment.find(make_symbol(t_sym_name)); }

    const std::string &get_home() const { return m_home_directory; }

    void interactive();

    void handle_signal(int t_c)
    {
        AL_DEBUG("Receiving signal: "s += std::to_string(t_c));
        m_evaluator.handle_signal(t_c);
    };

    const std::vector<std::string> get_symbols() const
    {
        std::vector<std::string> vec;

        for (auto [name, _] : env::Environment::g_global_symbol_table)
        {
            vec.push_back(name);
        }

        return vec;
    }

    const auto get_modules() const { return m_environment.get_modules(); }
};


}  // namespace alisp
