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

#include <type_traits>
#include <vector>
#include <iterator>
#include <utility>


#include "alisp/utility/vector_view.hpp"


namespace alisp::utility
{

template<typename T>
constexpr auto slice(T && t_iterable,
                     typename std::iterator_traits<decltype(std::begin(t_iterable))>::difference_type t_start = 0,
                     typename std::iterator_traits<decltype(std::begin(t_iterable))>::difference_type t_end = -1)
{
    const auto size =
        static_cast<typename std::iterator_traits<decltype(std::begin(t_iterable))>::difference_type>(std::size(t_iterable));
    auto start_it = std::next(std::begin(t_iterable), t_start);
    auto end_it = std::prev(std::end(t_iterable), t_end < 0 ? -t_end - 1 : size - t_end - 1);
    return std::make_pair(start_it, end_it);
}

template<typename T>
auto slice_vec(std::vector<T>& t_vec,
               typename std::iterator_traits<decltype(std::begin(t_vec))>::difference_type t_start = 0,
               typename std::iterator_traits<decltype(std::begin(t_vec))>::difference_type t_end = -1)
{
    auto [beg, end] = slice(t_vec, t_start, t_end);
    return std::vector<T>(beg, end);

}


template<typename T>
auto slice_view(std::vector<T>& t_vec,
               typename std::iterator_traits<decltype(std::begin(t_vec))>::difference_type t_start = 0,
               typename std::iterator_traits<decltype(std::begin(t_vec))>::difference_type t_end = -1)
{
    auto [beg, end] = slice(t_vec, t_start, t_end);
    return vector_view<T>(beg, end);

}



}
