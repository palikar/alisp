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


#include "alisp/alisp/alisp_engine.hpp"


namespace alisp
{

void LanguageEngine::do_eval(std::string &t_input, const std::string &t_file, bool t_print_res)
{
    auto parse_result = m_parser->parse(t_input, t_file);

    if (check(EngineSettings::OPTIMIZATION) or utility::env_bool(ENV_VAR_OPTIMIZE))
    {
        g_optimizer.optimize(parse_result);
    }

    for (auto sexp : parse_result)
    {
        if (check(EngineSettings::PARSER_DEBUG)) std::cout << "DEUBG[PARSER]: " << alisp::dump(sexp) << "\n";
        auto eval_result = m_evaluator.eval(sexp);
        if (check(EngineSettings::EVAL_DEBUG)) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_result) << "\n";
        if (t_print_res)
        {
            std::cout << *eval_result << "\n";
        }
    }
    
}

void LanguageEngine::load_init_scripts()
{
    AL_DEBUG("Loading init scripts"s);
    namespace fs = std::filesystem;

    if (fs::is_directory(prelude_directory))
    {
        AL_DEBUG("Loading directory: "s += prelude_directory);
        for (auto &al_file : fs::directory_iterator(prelude_directory))
        {
            AL_DEBUG("Loading file: "s += al_file.path().string());
            eval_file(al_file, false);
        }
    }

    auto alisprc = fs::path(m_home_directory) / ".alisprc";

    if (utility::env_bool(ENV_VAR_RC))
    {
        alisprc = fs::path(m_home_directory) / utility::env_string(ENV_VAR_RC);
    }

    if (fs::is_regular_file(alisprc))
    {
        AL_DEBUG("Loading file: "s += alisprc.string());
        eval_file(alisprc, false);
    }
}

void LanguageEngine::init_system()
{
    namespace fs = std::filesystem;

    AL_DEBUG("Initing the interpreter system."s);

    env::init_modules();
    al::init_streams();
    warnings::init_warning(m_warnings);

    env::update_prime(Qcommand_line_args, make_list(m_argv));
    AL_DEBUG("CLI arguments: "s += dump(Vcommand_line_args));

    const std::string al_path = utility::env_string(ENV_VAR_MODPATHS);
    const auto add_modules    = [&](auto &path) { Vmodpaths->children().push_back(make_string(path)); };
    if (!al_path.empty())
    {
        AL_DEBUG("ALPATH used: "s += al_path);
        auto paths = utility::split(al_path, ':');
        std::for_each(std::begin(paths), std::end(paths), add_modules);
    }
    std::for_each(std::begin(m_imports), std::end(m_imports), add_modules);

    Vcurrent_module->set("--main--");

    if (!check(EngineSettings::QUICK_INIT))
    {
        load_init_scripts();
    }

    Vdebug_mode = check(EngineSettings::DISABLE_DEBUG_MODE) or utility::env_bool(ENV_VAR_NODEBUG) ? Qnil : Qt;
}

std::pair<bool, int> LanguageEngine::eval_statement(std::string &command, bool exit_on_error)
{
    try
    {
        AL_DEBUG("Evaluating statement: "s += command);

        eval::detail::EvaluationLock lock{ m_evaluator };
        do_eval(command, "__EVAL__", true);

        return { true, 0 };
    }
    catch (al_exit &ex)
    {
        if (exit_on_error)
        {
            return { false, ex.value() };
        }
    }
    catch (...)
    {
        handle_errors_lippincott<false>();
        if (exit_on_error)
        {
            return { false, 0 };
        }
    }
    return { true, 0 };
}

std::pair<bool, int> LanguageEngine::eval_file(const std::filesystem::path &t_path, bool insert_mod_path)
{
    AL_DEBUG("Evaluating file: "s += t_path);
    m_evaluator.set_current_file(t_path);

    m_evaluator.reset_evaluation_flag();

    namespace fs = std::filesystem;
    if (insert_mod_path)
    {

        if (t_path.has_parent_path())
        {
            Vmodpaths->children().push_back(make_string(fs::absolute(t_path.parent_path())));
        }
        else
        {
            AL_DEBUG("Adding path to the modpaths: "s += fs::absolute(fs::current_path()).string());
            Vmodpaths->children().push_back(make_string(fs::absolute(fs::current_path())));
        }
    }

    try
    {
        {
            eval::detail::EvaluationLock lock{ m_evaluator };
            auto file_content = utility::load_file(t_path);
            do_eval(file_content, t_path);
        }

        while (m_evaluator.is_async_pending())
        {
            // std::unique_lock<std::mutex> lock(m_evaluator.callback_m);
            m_evaluator.async().spin_loop();
            m_evaluator.callback_cv.wait(m_evaluator.lock());

            if (!m_evaluator.is_interactive())
            {
                m_evaluator.dispatch_callbacks();
            }
        }
    }
    catch (al_exit &ex)
    {
        return { false, ex.value() };
    }
    catch (...)
    {
        handle_errors_lippincott<true>();
    }
    return { true, 0 };
}

std::pair<bool, int> LanguageEngine::eval_objs(std::vector<ALObjectPtr> t_objs)
{

    try
    {
        AL_DEBUG("Evaluating object");
        for (auto sexp : t_objs)
        {
            auto eval_result = m_evaluator.eval(sexp);
            if (check(EngineSettings::EVAL_DEBUG)) std::cout << "DEUBG[EVAL]: " << alisp::dump(eval_result) << "\n";
        }

        return { true, 0 };
    }
    catch (al_exit &ex)
    {
        return { false, ex.value() };
    }
    catch (...)
    {
        handle_errors_lippincott<false>();
        return { false, 0 };
    }
    return { true, 0 };
}

void LanguageEngine::interactive()
{
    Vmodpaths->children().push_back(make_string(utility::env_string("PWD")));
    m_evaluator.set_interactive_flag();
}

}  // namespace alisp
