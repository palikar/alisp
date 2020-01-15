#pragma once

#include <vector>
#include <string>
#include <iostream>
#include <string>
#include <optional>
	

namespace alisp::prompt
{


    
void init();
std::optional<std::string> repl(const std::string& prompt);

}
