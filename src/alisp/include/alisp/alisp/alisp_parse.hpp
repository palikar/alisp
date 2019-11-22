#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <cmath>
#include <unordered_map>
#include <array>
#include <tuple>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/utility/lite_string.hpp"


namespace alisp
{


namespace detail
{

enum Alphabet
{
		id_alphabet = 0,
		whitespace_alphabet,
};

constexpr size_t LENGTHOF_ALPHABET = 256;
constexpr size_t ALPHABETS_NUM = 2;

struct Position
{
		constexpr Position() = default;

		constexpr Position(const char * t_pos, const char * t_end) noexcept
				: line(1), col(1), pos(t_pos), end(t_end), last_col(1)
		{
		}

		static std::string_view str(const Position &begin, const Position &end) noexcept {

				if (begin.pos != nullptr && end.pos != nullptr) {
						return std::string_view(begin.pos,
																		static_cast<size_t>(std::distance(begin.pos, end.pos)));
				} else {
						return {};
				}
		}

		constexpr Position &operator++() noexcept {
				if (pos != end) {
						if (*pos == '\n') {
								++line;
								last_col = col;
								col = 1;
						} else {
								++col;
						}

						++pos;
				}
				return *this;
		}

		constexpr Position &operator--() noexcept {
				--pos;
				if (*pos == '\n') {
						--line;
						col = last_col;
				} else {
						--col;
				}
				return *this;
		}

		constexpr const char& operator*() const noexcept {
				if (pos == end) {
						return ""[0];
				} else {
						return *pos;
				}
		}

		constexpr bool operator==(const Position &rhs) const noexcept {
				return pos == rhs.pos;
		}

		constexpr bool operator!=(const Position &rhs) const noexcept {
				return pos != rhs.pos;
		}


		constexpr Position &operator+=(size_t distance) noexcept {
				*this = (*this) + distance;
				return *this;
		}

		constexpr Position operator+(size_t distance) const noexcept {
				Position ret(*this);
				for (size_t i = 0; i < distance; ++i) {
						++ret;
				}
				return ret;
		}


		constexpr bool has_more() const noexcept {
				return pos != end;
		}

		constexpr size_t remaining() const noexcept {
				return static_cast<size_t>(end - pos);
		}



		int line = -1;
		int col = -1;

	private:
		const char *pos = nullptr;
		const char *end = nullptr;
		int last_col = -1;

};


struct DepthTracker
{
	DepthTracker(size_t& depth) :m_depth(++depth) {} ;
	~DepthTracker() { --m_depth; }

	private:
		size_t& m_depth;
	};

}


namespace parser
{
		
	template <class ErrorHandler, class Environment>
	class ALParser : public ParserBase
	{
	private:

		detail::Position position;
		size_t depth;
		const ErrorHandler& err;
		Environment& env;


		constexpr static utility::LiteString cr_lf{"\r\n"};
		constexpr static utility::LiteString nl{"\n"};

		template<typename Array2D, typename First, typename Second>
		constexpr static void set_alphabet(Array2D &array, const First first, const Second second) noexcept
			{
				auto *first_ptr = &std::get<0>(array) + static_cast<std::size_t>(first);
				auto *second_ptr = &std::get<0>(*first_ptr) + static_cast<std::size_t>(second);
				*second_ptr = true;
			}

		constexpr bool char_in_alphabet(char c, detail::Alphabet a) const noexcept
			{
				return this->alphabet[a][static_cast<uint8_t>(c)];
			}

		constexpr static std::array<std::array<bool, detail::LENGTHOF_ALPHABET>, detail::ALPHABETS_NUM> build_alphabet() noexcept
			{
				std::array<std::array<bool, detail::LENGTHOF_ALPHABET>, detail::ALPHABETS_NUM> alph{};

				set_alphabet(alph, detail::whitespace_alphabet, ' ');
				set_alphabet(alph, detail::whitespace_alphabet, '\t');
				set_alphabet(alph, detail::whitespace_alphabet, '\n');

				for ( size_t c = 'a' ; c <= 'z' ; ++c ) {set_alphabet(alph, detail::id_alphabet, c);}
				for ( size_t c = 'A' ; c <= 'Z' ; ++c ) {set_alphabet(alph, detail::id_alphabet, c);}
				for ( size_t c = '0' ; c <= '9' ; ++c ) {set_alphabet(alph, detail::id_alphabet, c);}
				set_alphabet(alph, detail::id_alphabet, '-');
				set_alphabet(alph, detail::id_alphabet, '_');
				set_alphabet(alph, detail::id_alphabet, '+');
				set_alphabet(alph, detail::id_alphabet, '-');
				set_alphabet(alph, detail::id_alphabet, '*');
				set_alphabet(alph, detail::id_alphabet, '/');
				set_alphabet(alph, detail::id_alphabet, '=');
				set_alphabet(alph, detail::id_alphabet, '$');
				set_alphabet(alph, detail::id_alphabet, '%');
				set_alphabet(alph, detail::id_alphabet, '?');
				set_alphabet(alph, detail::id_alphabet, '|');
				set_alphabet(alph, detail::id_alphabet, '@');


				return alph;
			}

		constexpr static auto alphabet = build_alphabet();

	public:


		explicit ALParser(const ErrorHandler& err_, Environment& env_) : err(err_), env(env_)
			{

			}


		ALParser(const ALParser&) = delete;
		ALParser(const ALParser&&) = default;
		ALParser &operator=(const ALParser&) = delete;
		ALParser &operator=(ALParser&&) = delete;

	private:

		void skip_whitespace(){

			while (this->position.has_more() && char_in_alphabet(*position, detail::whitespace_alphabet))
			{
				++position;
			}
		}

		void skip_eol(){
			while(position.has_more() && check_char(*nl.c_str()))
			{
				++position;
			}
		}

		bool check_char(const char c)
			{
				return *this->position == c;
			}

		bool check_num()
			{
			
				if (check_char('x') || check_char('b') || check_char('o')) {
					position++;
				}

				if (check_char('-') || check_char('+')) {
					position++;
				}


				bool dot_found = false;
				bool e_found = false;
				bool e_minus_found = false;
				while (this->position.has_more() && !char_in_alphabet(*position, detail::whitespace_alphabet))
				{

					if (check_char('.') && !dot_found) {
						dot_found = true;
					} else {
						return false;
					}

					if (check_char('e') && !e_found) {
						e_found = true;
					} else {
						return false;
					}

					if (check_char('-') && !e_minus_found && e_found) {
						e_minus_found = true;
					} else {
						return false;
					}

					if (std::isdigit(*position)) {
						return false;
					}
				}
			
				return true;
			}

		ALObject* parse_id()
			{
				auto temp = this->position;
				while(this->position.has_more() && char_in_alphabet(*this->position, detail::id_alphabet))
				{
					++this->position;
				}

				const auto word = detail::Position::str(temp, this->position);
				return make_symbol(word);
			}

		ALObject* parse_string()
			{

				if(*this->position != '\"') { return nullptr; }

				++this->position;

				auto temp = this->position;

				while(this->position.has_more() && *this->position != '\"' )
				{
					++this->position;
					if ( *this->position == '\\') { this->position += 2; }
				}

				if (*this->position != '\"')
				{
					this->err.lexer_error(static_cast<size_t>(this->position.col),
																static_cast<size_t>(this->position.line),
																"Invalid string literal");
				}
				const auto text = detail::Position::str(temp, this->position);
				++this->position;

				return make_string(text);
				
			}

		ALObject* parse_number()
			{
				int sign = 1;
				bool real = false;

				auto t = this->position;
				if (*this->position == '-')
				{
					sign = -1;
					++this->position;
				}

				if (*this->position == '+') { ++this->position; }

				if (!std::isdigit(*this->position) && *this->position != '.')
				{
					this->position = t;
					return nullptr;
				}

				auto temp = this->position;
				if (*this->position == '.')
				{
					++this->position;
					real = true;
				}

				while(this->position.has_more() && std::isdigit(*this->position))
				{
					++this->position;
					if (*this->position == '.')
					{
						if (real)
						{
							this->err.lexer_error(static_cast<size_t>(this->position.col),
																		static_cast<size_t>(this->position.line),
																		"Invalid number.");
						}
						real = true;
						++this->position;
					}
				}

				const auto res = detail::Position::str(temp, this->position);

				if (real) {
					float num = static_cast<float>(sign) * std::stof( std::string(res) );
					return make_double(num);
				} else {
					int num = sign * std::stoi( std::string(res) );
					return make_int(num);
				}


			}

		ALObject* parse_quote()
			{
				position++;
				skip_whitespace();
				// TODO : check char here
				auto obj = parse_next();
				// TODO : check nulltpr here
				return make_object(make_symbol("quote"), obj);
			}

		ALObject* parse_backquote()
			{
				position++;
				skip_whitespace();
				// TODO : check char here
				auto obj = parse_next();
				// TODO : check nulltpr here
				return make_object(make_symbol("\`"), obj);
			}

		ALObject* parse_hashtag()
			{
				// TODO : check char here
				position++;
				// TODO : check char here
				position++;
				skip_whitespace();
				auto obj = parse_next();
				// TODO : check nulltpr here
				return make_object(make_symbol("function"), obj);
			
				return nullptr;
			}

		ALObject* parse_list()
			{
				// TODO : check char here
				position++;
				skip_whitespace();

				if(check_char(')')){
					position++;
					return make_smbol("nil");
				}

				std::vector<ALObject*> objs;
				while(true){

					auto next_obj = parse_next();
					if(next_obj){
						objs.emplace_back(next_obj);
					} else {
						// TODO : error here!
						return nullptr;
					}

					skip_whitespace();
					
					if(check_char(')')){
						position++;
						break;
					}

				
				}

				return make_object(objs);

			}

		ALObject* parse_comma()
			{
				// TODO : check char here
				position++;
				if (check_char('@')) {
					position++;
					skip_whitespace();
					auto obj = parse_next();
					// TODO : check nulltpr here
					return make_object(make_symbol(","), obj);				
				} else {
					skip_whitespace();
					auto obj = parse_next();
					// TODO : check nulltpr here
					return make_object(make_symbol(","), obj);
				}
			}

		ALObject* parse_next()
			{
				detail::DepthTracker dt{depth}
skip_whitespace();
			if(!position.has_more()) return nullptr;

			if ( *position =='('  ) return parse_list();
			if ( *position == '\"') return parse_string();
			if ( *position == '\'') return parse_quote();
			if ( *position == ',' ) return parse_comma();
			if ( *position == '\'') return parse_quote();
			if ( *position == '\`') return parse_backquote();
			if ( *position == '#') return parse_hashtag();
			if (  check_num()     ) return parse_number();

			return parse_id();

		}


	public:
			
			std::vector<ALObject*> parse(const std::string& input, const std::string& file_name) override
				{
					std::vector<ALObject*> objects;

					const auto begin = input.empty() ? nullptr : &input.front();
					const auto end = begin == nullptr ? nullptr : begin + input.size();
					this->position = detail::Position(begin, end);
					depth = 0;

					while(position.has_more())
					{
						auto next_object = parse_next();
						if (next_object)
						{
							objects.emplace_back(next_object);
						}
					}

					return objects;

				}


};

}

}
