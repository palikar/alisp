#pragma once


#include <tuple>
#include <type_traits>


namespace alisp::utility
{


template<std::size_t I = 0, typename FuncT, typename... Tp>
inline std::enable_if_t<I == sizeof...(Tp), void>
for_each(std::tuple<Tp...> &, FuncT)
{ }

template<std::size_t I = 0, typename FuncT, typename... Tp>
inline std::enable_if_t<I < sizeof...(Tp), void>
for_each(std::tuple<Tp...>& t, FuncT f)
{
    f(std::get<I>(t));
    for_each<I + 1, FuncT, Tp...>(t, f);
}


    
template <typename T>
struct reversion_wrapper { T& iterable; };

template <typename T>
auto begin (reversion_wrapper<T> w) { return std::rbegin(w.iterable); }

template <typename T>
auto end (reversion_wrapper<T> w) { return std::rend(w.iterable); }


template <typename T>
reversion_wrapper<T> reverse (T&& iterable) { return { iterable }; }


}
