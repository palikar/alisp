#pragma once

#include <type_traits>
#include <iterator>
#include <vector>
#include <utility>


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

}
