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
#include <sstream>
#include <string_view>


namespace alisp::utility
{


struct LiteString
{
    template<size_t N> constexpr LiteString(const char (&str)[N]) noexcept : m_size(N - 1), data(&str[0]) {}

    constexpr size_t size() const noexcept { return m_size; }

    constexpr const char *c_str() const noexcept { return data; }

    constexpr auto begin() const noexcept { return data; }

    constexpr auto end() const noexcept { return data + m_size; }

    constexpr bool operator==(const std::string_view &other) const noexcept
    {
        auto b1       = begin();
        const auto e1 = end();
        auto b2       = other.begin();
        const auto e2 = other.end();

        if (e1 - b1 != e2 - b2) { return false; }

        while (b1 != e1)
        {
            if (*b1 != *b2) { return false; }
            ++b1;
            ++b2;
        }
        return true;
    }

    bool operator==(const std::string &t_str) const noexcept
    {
        return std::equal(begin(), end(), std::cbegin(t_str), std::cend(t_str));
    }

    const size_t m_size;
    const char *data = nullptr;
};


}  // namespace alisp::utility
