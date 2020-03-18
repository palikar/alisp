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
#include "alisp/alisp/alisp_engine.hpp"

using namespace alisp;

void got_signal(int)
{
}


int main(int argc, char *argv[])
{
    using namespace alisp;
    namespace fs = std::filesystem;

    env::Environment m_environment;
    parser::ALParser<env::Environment> m_parser{ m_environment };
    eval::Evaluator m_evaluator(m_environment, &m_parser);


    const std::string m_home_directory = utility::env_string("HOME");

    std::vector<std::string> args{};
    args.reserve(static_cast<size_t>(argc));
    for (int i = 1; i < argc; ++i) { args.push_back(argv[i]); }

    env::init_modules();
    al::init_streams();
    warnings::init_warning({});

    env::update_prime(Qcommand_line_args, make_list(args));

    const std::string al_path = utility::env_string(ENV_VAR_MODPATHS);
    const auto add_modules    = [&](auto &path) { Vmodpaths->children().push_back(make_string(path)); };
    if (!al_path.empty())
    {
        auto paths = utility::split(al_path, ':');
        std::for_each(std::begin(paths), std::end(paths), add_modules);
    }

    Vcurrent_module->set("--main--");
    Vdebug_mode = utility::env_bool(ENV_VAR_NODEBUG) ? Qnil : Qt;


    if (fs::is_directory(prelude_directory))
    {
        for (auto &al_file : fs::directory_iterator(prelude_directory))
        { m_evaluator.eval_file(al_file.path().string()); }
    }

    auto alisprc = fs::path(m_home_directory) / ".alisprc";
    if (utility::env_bool(ENV_VAR_RC)) { alisprc = fs::path(m_home_directory) / utility::env_string(ENV_VAR_RC); }
    if (fs::is_regular_file(alisprc)) { m_evaluator.eval_file(alisprc); }


    auto sym_0 = make_list(
      make_symbol("defun"),
      make_symbol("do-it"),
      make_list(make_symbol("a")),
      make_list(make_symbol("println"), make_symbol("a")),
      make_list(
        make_symbol("let"),
        make_list(make_list(make_symbol("a"), make_int(42))),
        make_list(make_symbol("println"), make_symbol("a")),
        make_list(make_symbol("let"),
                  make_list(make_list(make_symbol("a"), make_list(make_symbol("+"), make_symbol("a"), make_int(1)))),
                  make_list(make_symbol("println"), make_symbol("a"))),
        make_list(make_symbol("println"), make_symbol("a"))),
      make_list(make_symbol("println"), make_symbol("a")));
    m_evaluator.eval(std::move(sym_0));

    auto sym_1 = make_list(make_symbol("defvar"), make_symbol("a"), make_int(10));
    m_evaluator.eval(std::move(sym_1));

    auto sym_2 = make_list(make_symbol("println"), make_symbol("a"));
    m_evaluator.eval(std::move(sym_2));

    auto sym_3 = make_list(make_symbol("do-it"), make_symbol("a"));
    m_evaluator.eval(std::move(sym_3));

    auto sym_4 = make_list(make_symbol("println"), make_symbol("a"));
    m_evaluator.eval(std::move(sym_4));

    auto sym_5 = make_list(make_symbol("dump"), make_symbol("--argv--"));
    m_evaluator.eval(std::move(sym_5));
}
