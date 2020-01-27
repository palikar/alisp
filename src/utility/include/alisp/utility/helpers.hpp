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


#include <tuple>
#include <type_traits>


namespace alisp::utility
{


template<std::size_t I = 0, typename FuncT, typename... Tp> inline std::enable_if_t<I == sizeof...(Tp), void> for_each(std::tuple<Tp...> &, FuncT)
{
}

template<std::size_t I = 0, typename FuncT, typename... Tp> inline std::enable_if_t < I<sizeof...(Tp), void> for_each(std::tuple<Tp...> &t, FuncT f)
{
    f(std::get<I>(t));
    for_each<I + 1, FuncT, Tp...>(t, f);
}


template<typename T> struct reversion_wrapper
{
    T &iterable;
};

template<typename T> auto begin(reversion_wrapper<T> w)
{
    return std::rbegin(w.iterable);
}

template<typename T> auto end(reversion_wrapper<T> w)
{
    return std::rend(w.iterable);
}


template<typename T> reversion_wrapper<T> reverse(T &&iterable)
{
    return { iterable };
}


}  // namespace alisp::utility
