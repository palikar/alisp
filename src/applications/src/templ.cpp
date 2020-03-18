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

alisp::LanguageEngine *g_alisp_engine = nullptr;

void got_signal(int c)
{

    if (g_alisp_engine)
    {

        alisp::prompt::SaveHistory hist;

        try
        {
            g_alisp_engine->handle_signal(c);
        }
        catch (...)
        {
            alisp::handle_errors_lippincott<false>();
        }
    }
}

int main(int argc, char *argv[])
{


    std::vector<alisp::EngineSettings> settings;
    settings.reserve(4);

    // settings.push_back(alisp::EngineSettings::EVAL_DEBUG);
    // settings.push_back(alisp::EngineSettings::PARSER_DEBUG);
    // settings.push_back(alisp::EngineSettings::QUICK_INIT);
    // settings.push_back(alisp::EngineSettings::DISABLE_DEBUG_MODE);

    alisp::LanguageEngine alisp_engine{settings};
    g_alisp_engine = &alisp_engine;

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = got_signal;
    sigfillset(&sa.sa_mask);
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);
