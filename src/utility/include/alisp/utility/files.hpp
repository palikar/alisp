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

#include <cstdint>
#include <filesystem>
#include <fstream>
#include <vector>
#include <string>
#include <cassert>
#include <cstring>


namespace alisp::utility
{

inline bool skip_bom(std::ifstream &infile)
{
    size_t bytes_needed = 3;
    char buffer[3];

    memset(buffer, '\0', bytes_needed);

    infile.read(buffer, static_cast<std::streamsize>(bytes_needed));

    if ((buffer[0] == '\xef') && (buffer[1] == '\xbb') && (buffer[2] == '\xbf'))
    {

        infile.seekg(3);
        return true;
    }

    infile.seekg(0);

    return false;
}

inline std::string load_file(const std::string &t_filename)
{
    std::ifstream infile(t_filename.c_str(), std::ios::in | std::ios::ate | std::ios::binary);

    if (!infile.is_open()) {}

    auto size = infile.tellg();
    infile.seekg(0, std::ios::beg);

    assert(size >= 0);

    if (skip_bom(infile))
    {
        size -= 3;
        assert(size >= 0);
    }

    if (size == std::streampos(0)) { return std::string(); }
    else
    {
        std::vector<char> v(static_cast<size_t>(size));
        infile.read(&v[0], static_cast<std::streamsize>(size));
        return std::string(v.begin(), v.end());
    }
}

}  // namespace alisp::utility
