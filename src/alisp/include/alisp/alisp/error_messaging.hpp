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
#include <vector>
#include <stdexcept>


#include "alisp/alisp/alisp_common.hpp"


namespace alisp
{


class ErrorMessanger
{
  private:
    std::string current_file;
    std::string current_input;
    std::vector<std::string> lines;

    bool file_input = false;

  public:
    ErrorMessanger() = default;


    void set_input(std::string input);
    void set_file(std::string file);


    // void lexer_error(size_t char_num,
    //                  size_t line_num,
    //                  const std::string& msg) const;


    // void parser_error(const lexer::ALToken& token,
    //                   const std::string& msg) const;

    // void runtime_error(const std::string& msg) const;
};


class ThrowingMessanger : public ErrorMessanger
{
  private:
  public:
    ThrowingMessanger() = default;

    // void lexer_error(size_t char_num,
    //                  size_t line_num,
    //                  const std::string& msg) const;

    // void parser_error(const lexer::ALToken& token,
    //                   const std::string& msg) const;

    // void runtime_error(const std::string& msg) const;
};


}  // namespace alisp
