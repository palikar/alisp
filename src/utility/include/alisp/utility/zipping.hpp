#pragma once

#include <type_traits>
#include <iterator>

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

}
