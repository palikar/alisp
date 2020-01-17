#pragma once

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
