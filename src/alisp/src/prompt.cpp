#include <iostream>

#include "alisp/alisp/prompt.hpp"

#include <readline/readline.h>
#include <readline/history.h>


namespace alisp::prompt
{

std::vector<std::string> matches;
size_t match_index = 0;
    
std::vector<std::string> get_completions(const std::string& )
{
    return {};
}

char* completion_generator(const char* text, int state)
{

    if (state == 0) {
        matches.clear();
        match_index = 0;

        std::string textstr{text};
        for (const auto& word : get_completions(text)) {
            if (word.size() >= textstr.size() &&
                word.compare(0, textstr.size(), textstr) == 0) {
                matches.push_back(word);
            }
        }
            
    }

    if (match_index >= matches.size()) {
        return nullptr;
    } else {
        return strdup(matches[match_index++].c_str());
    }
}

char** completer(const char* text, int, int )
{
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, completion_generator);
}

void init()
{
    rl_attempted_completion_function = completer;


    
}

void repl(const std::string& prompt)
{
    char* buf;
    while ((buf = readline(prompt.c_str())) != nullptr) {
        if (strlen(buf) > 0 && *buf != ' ') {
            add_history(buf);
        }

        const std::string input{buf};

        // use the engine here
        std::cout << input << "\n";

        free(buf);
    }            
}

}
