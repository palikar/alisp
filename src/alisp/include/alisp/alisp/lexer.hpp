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

#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/error_messaging.hpp"

#include "alisp/common/lite_string.hpp"


namespace alisp
{


namespace inner {

	enum Alphabet
	{
    symbol_alphabet = 0,
    id_alphabet,
    keyword_alphabet,
    whitespace_alphabet,
    max_alphabet,
    lengthof_alphabet = 256
	};


	struct Postion()
	{
		constexpr Position() = default;

		constexpr Position(const char * t_pos, const char * t_end) noexcept
			: line(1), col(1), m_pos(t_pos), m_end(t_end), m_last_col(1)
		{
		}

		static std::string_view str(const Position &begin, const Position &end) noexcept {
					
			if (begin.m_pos != nullptr && end.m_pos != nullptr) {
				return std::string_view(begin.m_pos, std::distance(begin.m_pos, end.m_pos));
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

				++m_pos;
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
			return pos != rhs.m_pos;
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
				
	}
	
}


template <class ErrorHandler>
class ALLexer
{
  private:
	  inner::Position position;

    const ErrorHandler& err;

    
    
    constexpr static LiteString keyword_start{"&"};
	  constexpr static LiteString cr_lf{"\r\n"};
	  constexpr static LiteString nl{"\n"};

	
	  constexpr static std::array<LiteString, 2> create_keywords() noexcept
    {
        std::array<LiteString, 2> keywords_vec = {{
                LiteString{"optional"},
                LiteString{"rest"}
            }};
        return keywords_vec;
    }

	  constexpr static std::unordered_map<char, TokenType> symbols_map() noexcept
    {
        std::unordered_map<char, TokenType> symbols_vec = {{
                {'(', TokenType::LEFT_BRACE},
                {')',TokenType::RIGHT_BRACE},
                {'{',TokenType::LEFT_BRACKET},
                {'}',TokenType::RIGHT_BRACKET},
                {'@', TokenType::AT},
                {'\'',TokenType::QUOTE}
            }};
        return symbols_vec;
    }

    template<typename Array2D, typename First, typename Second>
    constexpr static void set_alphabet(Array2D &array, const First first, const Second second) noexcept
    {
        auto *first_ptr = &std::get<0>(array) + static_cast<std::size_t>(first);
        auto *second_ptr = &std::get<0>(*first_ptr) + static_cast<std::size_t>(second);
        *second_ptr = true;
    }

    constexpr bool char_in_alphabet(char c, detail::Alphabet a) const noexcept
		{ 
        return m_alphabet[a][static_cast<uint8_t>(c)]; 
    }
    
    constexpr static std::array<std::array<bool, inner::lengthof_alphabet>, inner::max_alphabet> build_alphabet() noexcept
    {
        std::array<std::array<bool, inner::lengthof_alphabet>, inner::max_alphabet> alph{};

        set_alphabet(alph, inner::symbol_alphabet, '(');
        set_alphabet(alph, inner::symbol_alphabet, ')');
        set_alphabet(alph, inner::symbol_alphabet, '{');
        set_alphabet(alph, inner::symbol_alphabet, '}');
        set_alphabet(alph, inner::symbol_alphabet, '@');
        set_alphabet(alph, inner::symbol_alphabet, '\'');

        set_alphabet(alph, inner::whitespace_alphabet, ' ');
        set_alphabet(alph, inner::whitespace_alphabet, '\t');

        for ( size_t c = 'a' ; c <= 'z' ; ++c ) {set_alphabet(alph, inner::id_alphabet, c);}
        for ( size_t c = 'A' ; c <= 'Z' ; ++c ) {set_alphabet(alph, inner::id_alphabet, c);}
        for ( size_t c = '0' ; c <= '9' ; ++c ) {set_alphabet(alph, inner::id_alphabet, c);}
        set_alphabet(alph, inner::id_alphabet, '-');
        set_alphabet(alph, inner::id_alphabet, '_');
        set_alphabet(alph, inner::id_alphabet, '+');
        set_alphabet(alph, inner::id_alphabet, '-');
        set_alphabet(alph, inner::id_alphabet, '*');
        set_alphabet(alph, inner::id_alphabet, '/');
        set_alphabet(alph, inner::id_alphabet, '=');
        set_alphabet(alph, inner::id_alphabet, '$');
        set_alphabet(alph, inner::id_alphabet, '%');
        set_alphabet(alph, inner::id_alphabet, '?');
        set_alphabet(alph, inner::id_alphabet, '|');
        
        for ( size_t c = 'a' ; c <= 'z' ; ++c ) {set_alphabet(alph, inner::keyword_alphabet, c);}
        for ( size_t c = 'A' ; c <= 'Z' ; ++c ) {set_alphabet(alph, inner::keyword_alphabet, c);}
        for ( size_t c = '0' ; c <= '9' ; ++c ) {set_alphabet(alph, inner::keyword_alphabet, c);}
        set_alphabet(alph, inner::keyword_alphabet, '-');
        set_alphabet(alph, inner::keyword_alphabet, '_');
            

        
        return alph;
    }

    constexpr static auto alphabet = build_alphabet();
	  constexpr static auto keywords = create_keywords();

  public:

    
	explicit ALLexer(const ErrorHandler& err_) : err(err_)
    {
    }

    ALLexer(const ALLexer&) = delete;
	  ALLexer(const ALLexer&&) = delete;
	  ALLexer &operator(const ALLexer&) = delete;
	  ALLexer &operator(ALLexer&&) = delete;


    void skip_whitespace(){
			while (this->position.has_more() && char_in_alphabet(*this->position, inner::whitespace_alphabet))
			{
				++this->position;
			}
    }

	  void skip_eol(){
			while(this->position.has_more() && check_char(*this->nl.c_str()))
			{
				++this->postition;
			}
		}
    
    bool capture_symbol(std::vector<alisp::ALToken>& tokens)
    {
			
        if(char_in_alphabet(*this->postion, inner::symbol_alphabet))
        {
            tokens.push_back(alisp::ALToken(symbols_map[*this->postion];, this->char_num, this->line_num));
					  ++this->postion;
            return true;
        }
        return false;
    }

    bool capture_keyword(std::vector<alisp::ALToken>& tokens)
    {
			if (!check_char(*keyword_start.c_str())) { return false;}
			++this->position;
			auto temp = this->postion;
			while(this->postion.has_more() && char_in_alphabet(*this->postion, inner::keyword_alphabet))
			{
				++this->postion;
			}
			const auto word = inner::Position::str(temp, this->postion);

			for (const auto& keyword : keywords)
			{
				if (word  == keyword)
				{
					tokens.push_back(alisp::ALToken(TokenType::ID, std::move(word), this->char_num,this->line_num));
					return true;
				}
			}
				
			return false;
    }

    bool capture_id(std::vector<alisp::ALToken>& tokens)
    {
        auto temp = this->postion;
        while(this->postion.has_more() && char_in_alphabet(*this->postion, inner::id_alphabet))
        {
            ++this->postion;
        }
        std::string word{temp, this->postion};
        tokens.push_back(alisp::ALToken(TokenType::ID, std::move(word), this->char_num, this->line_num));

				return true;
    }

	  bool capture_string(std::vector<alisp::ALToken>& tokens)
		{
			if(*this->position != '\"') return false;
			++this->position;
			auto temp = this->position;
			while(this->position.has_more() && *this->position != '\"' )
			{
				if ( *this->position == '\\') { this->position += 2; }
			}

			if (*this->position != '\"')
			{
				this->err.lexer_error(this->position.col, this->position.line, "Invalid string literal");
			}

			const auto text = inner::Position::str(temp, this->position);
			tokens.push_back(alisp::ALToken(TokenType::STRING, std::move(text)));			
			return true;
		}
	
	  bool capture_num(std::vector<alisp::ALToken>& tokens)
		{
			int sign = 1;
			bool real = false;
			bool retval = true;

			if (*this->position == '-')
			{
				sign = -1;
				this->position++;
			}

			if (*this->position == '+') { this->position++; }

			if (!std::isdigit(*this->position) && *this->position != '.') { return false; }

			auto temp = this->position;
			if (*this->position == '.') { real = true; }

			while(this->position.has_more() && std::isdigit(*this->position))
			{
				++this->position;
				if (*this->position == '.') {
					real = true;
					++this->position;
				}
			}

			const auto res = inner::Postion::str(temp, this->position);

			if (real)
			{
				float num = sign * std::stof(res);
				tokens.push_back(alisp::ALToken(TokenType::REAL_NUMBER, num,this->char_num, this->line_num));
			}
			else
			{
			  int num = sign * std::stoi(res);
				tokens.push_back(alisp::ALToken(TokenType::NUMBER, static_cast<int>(num), this->char_num, this->line_num));
			}

			return retval;
		}

    inline bool check_char(const char c)
    {
        return *input == c;
    }

	  std::vector<alisp::ALToken> tokenize(const std::string& input)
    {
			  std::vector<alisp::ALToken> tokens;
			  const auto begin = input.empty() ? nullptr : &input.front();
        const auto end = begin == nullptr ? nullptr : begin + input.size();
				this->position = inner::Position{begin, end};

				while(position.has_more())
				{
					skip_whitespace();
					skip_eol();

					if (capture_symbol(tokens)) { continue; }

					if (capture_keyword(tokens)) { continue; }

					if (capture_string(tokens)) { continue; }

					if (capture_num(tokens)) { continue; }

					if (capture_id(tokens)) { continue; }
					
					// this->err.lexer_error(this->char_num, this->line_num, "Invalid numer");
					
        }
				
        return tokens;
  
    };


}
