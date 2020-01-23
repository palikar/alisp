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

#include <iterator>
#include <functional>

namespace alisp::utility {

template <typename ... Iterators>
void advance_all (Iterators& ... iterators) {
    (std::next(iterators), ...);
} 
template <typename Function, typename Iterator, typename ... Iterators>
Function zip (Function func, Iterator begin, 
              Iterator end, 
              Iterators ... iterators)
{
    for(;begin != end; ++begin, advance_all(iterators...)) func(*begin, *(iterators)... );
    return func;
}


template <typename InType,
          template <typename U, typename alloc = std::allocator<U>>
          class InContainer,
          template <typename V, typename alloc = std::allocator<V>>
          class OutContainer = InContainer,
          typename OutType = InType>
OutContainer<OutType> mapf(const InContainer<InType>& input,
                           std::function<OutType(const InType&)> func)
{
    OutContainer<OutType> output;
    output.resize(input.size());
    transform(input.begin(), input.end(), output.begin(), func);
    return output;
}


}
