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
