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

#include <string>
#include <cstdlib>

namespace alisp::utility
{


inline bool env_bool(const char *t_name) { return std::getenv(t_name) != nullptr; }

inline std::string env_string(const char *t_name)
{
    auto e =  std::getenv(t_name) ;
    if (e != nullptr) { return std::string{ std::getenv(e) }; }
    return {};
}


}
