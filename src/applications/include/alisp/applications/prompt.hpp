#pragma once

#include <vector>
#include <string>



namespace alisp::prompt
{
    

std::vector<std::string> get_completions(const std::string& hint);

char* completion_generator(const char* text, int state);

char** completer(const char* text, int start, int end);

void init();

std::string repl(const std::string& prompt);

}
