#pragma once
#include <type_traits>


namespace nu::meta {

    template<class...>
    using void_t = void;


    template <typename T>
    struct crtp
    {
        T& underlying() { return static_cast<T&>(*this); }
        T const& underlying() const { return static_cast<T const&>(*this); }

    };
}
