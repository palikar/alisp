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


#include "alisp/alisp/alisp_parser.hpp"


namespace alisp
{

namespace detail
{

DepthTracker::DepthTracker(size_t &depth) : m_depth(++depth)
{
}
DepthTracker::~DepthTracker()
{
    --m_depth;
}


Position::Position(const char *t_pos, const char *t_end) noexcept : line(0), col(0), pos(t_pos), end(t_end), last_col(1)
{
}

std::string_view Position::str(const Position &begin, const Position &end) noexcept
{

    if (begin.pos != nullptr && end.pos != nullptr)
    {
        return std::string_view(begin.pos, static_cast<size_t>(std::distance(begin.pos, end.pos)));
    }
    else
    {
        return {};
    }
}

Position &Position::operator++() noexcept
{
    if (pos != end)
    {
        if (*pos == '\n')
        {
            ++line;
            last_col = col;
            col      = 1;
        }
        else
        {
            ++col;
        }

        ++pos;
    }
    return *this;
}

Position &Position::operator--() noexcept
{
    --pos;
    if (*pos == '\n')
    {
        --line;
        col = last_col;
    }
    else
    {
        --col;
    }
    return *this;
}

const char &Position::operator*() const noexcept
{
    if (pos == end)
    {
        return ""[0];
    }
    else
    {
        return *pos;
    }
}

bool Position::operator==(const Position &rhs) const noexcept
{
    return pos == rhs.pos;
}

bool Position::operator!=(const Position &rhs) const noexcept
{
    return pos != rhs.pos;
}

Position &Position::operator+=(size_t distance) noexcept
{
    *this = (*this) + distance;
    return *this;
}

Position Position::operator+(size_t distance) const noexcept
{
    Position ret(*this);
    for (size_t i = 0; i < distance; ++i)
    {
        ++ret;
    }
    return ret;
}

bool Position::has_more() const noexcept
{
    return pos != end;
}

size_t Position::remaining() const noexcept
{
    return static_cast<size_t>(end - pos);
}

}  // namespace detail

namespace parser
{

}


}  // namespace alisp
