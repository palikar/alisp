#include "alisp/alisp/alisp_engine.hpp"

#include <stdint.h>
#include <stddef.h>

#include <string>
#include <vector>
#include <iostream>


extern "C" int LLVMFuzzerTestOneInput(const uint8_t *data, std::size_t size) {

    alisp::LanguageEngine engine;
    
    char *str = new char[size+1];
    memcpy(str, data, size);

    std::string input{str, size};

    std::cout.setstate(std::ios_base::failbit);
    std::cerr.setstate(std::ios_base::failbit);
    
    auto t = engine.eval_statement(input);

    std::cout.clear();
    std::cerr.clear();

    delete[] str;
    
    return 0;
}
