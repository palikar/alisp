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

#include <sstream>

#include <clipp.hpp>
#include <fmt/format.h>

namespace fmt
{

template<> struct formatter<clipp::man_page>
{
    constexpr auto parse(format_parse_context &ctx)
    {
        auto it = ctx.begin();

        return it;
    }

    // Formats the point p using the parsed format specification (presentation)
    // stored in this formatter.
    template<typename FormatContext> auto format(const clipp::man_page &p, FormatContext &ctx)
    {

        std::stringstream ss;
        ss << p;
        return format_to(ctx.out(), "{}", ss.str());
    }
};

}  // namespace fmt
