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






#include "alisp/utility/defines.hpp"




namespace alisp
{

const std::string get_build_info() noexcept
{
    std::string build_str = fmt::format("ALisp {}.{}.{}\n",
                                        version_major, version_minor, version_patch);
    build_str += fmt::format("[{} {}] ", compiler_name, compiler_version);
    build_str += "Build:";
    build_str += debug_build ? " Debug":" Release";
    build_str += "\n";
    return build_str;

}

}
