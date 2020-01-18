#pragma once

#include <cmath>
#include <cctype>

namespace alisp::utility {

inline double round_nplaces(double value, const int64_t &to)
{

    int64_t places = 1;
    auto whole = static_cast<int64_t>(value);
    
    for(int64_t i = 0; i < to; i++) places *= 10;
    
    value -= static_cast<double>(whole);
    value *= static_cast<double>(places);
    value  = std::round(value);
    value /= static_cast<double>(places);
    value += static_cast<double>(whole);
    
    return value;
    
}


}
