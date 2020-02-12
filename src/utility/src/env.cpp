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

#include "alisp/utility.hpp"

extern "C" char **environ;


namespace alisp::utility
{

bool env_bool(const char *t_name)
{
    return std::getenv(t_name) != nullptr;
}

std::string env_string(const char *t_name)
{
    auto e = std::getenv(t_name);
    if (e != nullptr) { return std::string{ e }; }
    return {};
}


void env_set(const std::string &t_name, const std::string &t_val)
{
    std::string new_env = (t_name + '=' + t_val);

    putenv(new_env.data());
}

std::unordered_map<std::string, std::string> env_list()
{

    std::unordered_map<std::string, std::string> m;

    int i   = 1;
    char *s = *environ;

    for (; s; i++)
    {
        auto v = utility::split(s, '=');
        m.insert({ v[0], v[1] });
        s = *(environ + i);
    }

    return m;
}

}  // namespace alisp::utility
