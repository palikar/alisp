#pragma once

#include <type_traits>
#include <memory>


namespace alisp::utility {

template<class...>
using void_t = void;


template <typename T>
struct crtp
{
    T& underlying() { return static_cast<T&>(*this); }
    T const& underlying() const { return static_cast<T const&>(*this); }

};



template<typename T> struct is_shared_ptr : std::false_type {};
template<typename T> struct is_shared_ptr<std::shared_ptr<T>> : std::true_type {};
template<typename T> constexpr auto is_shared_ptr_v = is_shared_ptr<T>::value;

struct empty_base{};

}
