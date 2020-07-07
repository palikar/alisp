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

bool skip_bom(std::ifstream &infile);

bool skip_elf(std::ifstream &infile);

bool check_elf(const std::string &t_filename);

std::string load_file(const std::string &t_filename);

void dump_file(const std::string &t_filename, const std::string &t_content, bool t_append);

std::vector<unsigned char> load_file_binary(const std::string &t_filename);

}  // namespace alisp::utility
