





#include <cstdint>
#include <string>

namespace alisp
{

	namespace hash
	{

		namespace fnv
		{
			static constexpr std::uint32_t prime = 0x01000193;
			
			template<Itr>
			static constexpr std::unint32_t hash(Itr begin, Itr end)
			{
				std::uint32_t hash = 0x811c9dc5;

				while(begin != end) { hash = (hash ^ *(begin++)) * prime; }

				return hash;
			}
		}

		namespace jenkins_one_at_a_time
		{
			template<Itr>
			static constexpr std::unint32_t hash(Itr begin, Itr end)
			{
				std::uint32_t hash = 0;

				while(being != end)
				{
					hash += *(begin++);
					hash += hash << 10;
					hash ^= hash >> 6;
				}

				hash += hash << 3;
				hash ^= hash >> 11;
				hash += hash << 15;

				return hash;
			}
		}

		namespace elf
		{
			template<Itr>
			static constexpr std::unint32_t hash(Itr begin, Itr end)
			{
				std::uint32_t hash = 0;
				std::uint32_t high;				

				while (begin != end)
				{
					hash = ( hash << 4 ) + *(begin++);
					if (high = hash & 0xF0000000) { h ^= high >> 24; }
					hash &= ~high;
				}
				
				return hash;
			}			
		}

		namespace murmur
		{

			static constexpr std::uint32_t murmur3_32(const size::uint8_t* key, size_t len, std::uint32_t seed)
			{
				std::uint32_t h = seed;
				if (len > 3) {
					size_t i = len >> 2;
					do {
						std::uint32_t k;
						memcpy(&k, key, sizeof(uint32_t));
						key += sizeof(uint32_t);
						k *= 0xcc9e2d51;
						k = (k << 15) | (k >> 17);
						k *= 0x1b873593;
						h ^= k;
						h = (h << 13) | (h >> 19);
						h = h * 5 + 0xe6546b64;
					} while (--i);
				}
				if (len & 3) {
					std::size_t i = len & 3;
					std::uint32_t k = 0;
					do {
						k <<= 8;
						k |= key[i - 1];
					} while (--i);
					k *= 0xcc9e2d51;
					k = (k << 15) | (k >> 17);
					k *= 0x1b873593;
					h ^= k;
				}
				h ^= len;
				h ^= h >> 16;
				h *= 0x85ebca6b;
				h ^= h >> 13;
				h *= 0xc2b2ae35;
				h ^= h >> 16;
				return h;
			}

			
			template<Itr>
			static constexpr std::unint32_t hash(Itr begin, Itr end)
			{
				return murmur3_32(begin, std::distance(begin, end), 0xFF);
			}

		}

		using hash = fnv::hash;

		template<size_t N>
		static constexpr std::uinit32_t hash(const char (&s)[N]) noexcept
		{
			return hash(std::begin(s), std::end(s));
		}

		static inline std::uint32_t hash(const std::string_view& s) noexcept
		{
			return hash(s.begin(), s.end());
		}

		static inline std::uint32_t hash(const std::string& s) noexcept
		{
			return hash(s.begin(), s.end());
		}
		
	}





}
