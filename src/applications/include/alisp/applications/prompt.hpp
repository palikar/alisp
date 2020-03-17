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

#include <vector>
#include <string>
#include <iostream>
#include <string>
#include <optional>


namespace alisp::prompt
{

class SaveHistory
{
  public:
    SaveHistory() = default;
    ~SaveHistory();
    SaveHistory(SaveHistory &&) = default;
    SaveHistory &operator=(SaveHistory &&) = default;
    SaveHistory(const SaveHistory &)       = delete;
    SaveHistory &operator=(const SaveHistory &) = delete;
};

static std::string history_file;
void init(std::string history_file = "");
std::optional<std::string> repl(const std::string &prompt);

extern std::vector<std::string> get_completions(const std::string &);

}  // namespace alisp::prompt
