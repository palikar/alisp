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

#include <filesystem>
#include <fstream>

#include "alisp/applications/prompt.hpp"

#ifdef READLINE_AVAILABLE
#include <readline/readline.h>
#include <readline/history.h>
#endif


namespace alisp::prompt
{


#ifdef READLINE_AVAILABLE

std::vector<std::string> matches;
size_t match_index = 0;

char *completion_generator(const char *text, int state)
{

    if (state == 0)
    {
        matches.clear();
        match_index = 0;

        std::string textstr{ text };
        for (const auto &word : get_completions(text))
        {
            if (word.size() >= textstr.size() && word.compare(0, textstr.size(), textstr) == 0)
            {
                matches.push_back(word);
            }
        }
    }

    if (match_index >= matches.size())
    {
        return nullptr;
    }
    else
    {
        return strdup(matches[match_index++].c_str());
    }
}

char **completer(const char *text, int, int)
{
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, completion_generator);
}

void load_history()
{
    namespace fs = std::filesystem;

    if (history_file.empty())
    {
        return;
    }
    if (!fs::is_regular_file(history_file))
    {
        return;
    }

    std::ifstream alisphist(history_file);

    std::string line;
    while (std::getline(alisphist, line))
    {
        add_history(line.c_str());
    }
}


void init(std::string hist)
{
    rl_completer_quote_characters    = "\"'";
    rl_attempted_completion_function = completer;
    history_file                     = std::move(hist);
    using_history();
    if (!history_file.empty())
    {
        load_history();
    }
    rl_parse_and_bind(rl_brackets_compl);
    rl_parse_and_bind(rl_quote_compl);
}

std::optional<std::string> repl(const std::string &prompt)
{
    char *buf = readline(prompt.c_str());

    if (buf == nullptr)
    {
        free(buf);
        return {};
    }

    if (strlen(buf) > 0 && *buf != ' ')
    {
        add_history(buf);
    }
    std::string command{ buf };
    free(buf);

    return command;
}

SaveHistory::~SaveHistory()
{
    namespace fs = std::filesystem;

    if (history_file.empty())
    {
        return;
    }
    if (!fs::is_regular_file(history_file))
    {
        return;
    }

    auto hist_list = history_list();
    if (!hist_list)
    {
        return;
    }

    std::ofstream alisphist(history_file);
    for (size_t i = 0; hist_list[i]; i++)
    {
        alisphist << hist_list[i]->line << '\n';
    }

    alisphist.close();
}

#else

SaveHistory::~SaveHistory()
{
}
void init()
{
}

std::optional<std::string> repl(const std::string &prompt)
{
    std::string retval;
    std::cout << prompt;
    std::getline(std::cin, retval);
    return std::cin.eof() ? nullptr : retval;
}


#endif


}  // namespace alisp::prompt
