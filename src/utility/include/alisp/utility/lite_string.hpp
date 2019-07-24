#pragma once

#include <string>
#include <string_view>

namespace
{
namespace utility
{
		
struct LiteString
{
    template<size_t N>
    constexpr LiteString(const char (&str)[N]) noexcept
        : m_size(N-1), data(&str[0])
    {
    }

    constexpr size_t size() const noexcept {
        return m_size;
    }

    constexpr const char *c_str() const noexcept {
        return data;
    }

    constexpr auto begin() const noexcept {
        return data;
    }

    constexpr auto end() const noexcept {
        return data + m_size;
    }

    constexpr bool operator==(const std::string_view &other) const noexcept {
        auto b1 = begin();
        const auto e1 = end();
        auto b2 = other.begin();
        const auto e2 = other.end();

        if (e1 - b1 != e2 - b2) { return false; }

        while (b1 != e1) {
            if (*b1 != *b2) { return false; }
            ++b1; ++b2;
        }
        return true;
    }

    bool operator==(const std::string &t_str) const noexcept {
        return std::equal(begin(), end(), std::cbegin(t_str), std::cend(t_str));
    }

    const size_t m_size;
    const char *data = nullptr;
};
}

}

