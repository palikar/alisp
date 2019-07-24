#pragma once

#include <cstdint>
#include <string>

namespace alisp
{

namespace hash
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
    std::uint32_t high;				

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
[[nodiscard]] static constexpr std::uint32_t hash(const char (&s)[N]) noexcept
{
    return hash(std::begin(s), std::end(s)-1);
}

[[nodiscard]] static inline std::uint32_t hash(const std::string_view& s) noexcept
{
    return hash(s.begin(), s.end());
}

[[nodiscard]] static inline std::uint32_t hash(const std::string& s) noexcept
{
    return hash(s.begin(), s.end());
}
		
}

}