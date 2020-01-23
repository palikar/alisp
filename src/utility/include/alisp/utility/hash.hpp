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

#include <cstdint>
#include <string>

namespace alisp::hash
{

namespace fnv
{
static constexpr std::uint32_t prime = 0x01000193;
			
template<typename Itr>
static constexpr std::uint32_t hash(Itr begin, Itr end)
{
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif    

    std::uint32_t hash = 0x811c9dc5;

    while(begin != end) { hash = (hash ^ *(begin++)) * prime; }

    return hash;

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

}
}

namespace jenkins_one_at_a_time
{
template<typename Itr>
static constexpr std::uint32_t hash(Itr begin, Itr end)
{
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif

    std::uint32_t hash = 0;

    while(begin != end)
    {
        hash += *(begin++);
        hash += hash << 10;
        hash ^= hash >> 6;
    }

    hash += hash << 3;
    hash ^= hash >> 11;
    hash += hash << 15;

    return hash;

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

}
}

namespace elf
{
template<typename Itr>
static constexpr std::uint32_t hash(Itr begin, Itr end)
{
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif
    
    std::uint32_t hash = 0;
    std::uint32_t high = 0;				

    while (begin != end)
    {
        hash = ( hash << 4 ) + *(begin++);
        if ((high = (hash & 0xF0000000))) { hash ^= high >> 24; }
        hash &= ~high;
    }
				
    return hash;

    
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

}			
}


#ifdef ELF_HASHING
using elf::hash;
#elif JENKINS_HASHING
using jenkins_one_at_a_time::hash;
#else
using fnv::hash;
#endif


template<size_t N>
[[nodiscard]] inline constexpr std::uint32_t hash(const char (&s)[N]) noexcept
{
    return hash(std::begin(s), std::end(s)-1);
}

[[nodiscard]] inline std::uint32_t hash(const std::string_view& s) noexcept
{
    return hash(s.begin(), s.end());
}

[[nodiscard]] inline std::uint32_t hash(const std::string& s) noexcept
{
    return hash(s.begin(), s.end());
}
		


}
