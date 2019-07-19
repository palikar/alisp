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
#include "alisp/alisp/error_messaging.hpp"

#include "alisp/util/lite_string.hpp"


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

    constexpr static std::unordered_map<char, TokenType> symbols_map_gen() noexcept
    {
        std::unordered_map<char, TokenType> symbols_vec = {{
                {'(', TokenType::LEFT_BRACKET},
                {')',TokenType::RIGHT_BRACKET},
                {'@', TokenType::AT},
                {'\'',TokenType::QUOTE},
                {':',TokenType::COLON},
                {',',TokenType::COMMA},
                {'#',TokenType::HASHTAG},
                {'`',TokenType::BACKQUOTE}
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

    constexpr bool char_in_alphabet(char c, inner::Alphabet a) const noexcept
    { 
        return this->alphabet[a][static_cast<uint8_t>(c)]; 
    }
    
    constexpr static std::array<std::array<bool, inner::lengthof_alphabet>, inner::max_alphabet> build_alphabet() noexcept
    {
        std::array<std::array<bool, inner::lengthof_alphabet>, inner::max_alphabet> alph{};

        set_alphabet(alph, inner::symbol_alphabet, '(');
        set_alphabet(alph, inner::symbol_alphabet, ')');
        // set_alphabet(alph, inner::symbol_alphabet, '{');
        // set_alphabet(alph, inner::symbol_alphabet, '}');
        set_alphabet(alph, inner::symbol_alphabet, '@');
        set_alphabet(alph, inner::symbol_alphabet, '\'');
        set_alphabet(alph, inner::symbol_alphabet, ':');
        set_alphabet(alph, inner::symbol_alphabet, '#');
        set_alphabet(alph, inner::symbol_alphabet, ',');
        set_alphabet(alph, inner::symbol_alphabet, '`');

        set_alphabet(alph, inner::whitespace_alphabet, ' ');
        set_alphabet(alph, inner::whitespace_alphabet, '\t');
        set_alphabet(alph, inner::whitespace_alphabet, '\n');

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
    std::unordered_map<char, TokenType> symbols_map;

  public:

    
	explicit ALLexer(const ErrorHandler& err_) : err(err_)
    {
        this->symbols_map = ALLexer::symbols_map_gen();
    }

    ALLexer(const ALLexer&) = delete;
    ALLexer(const ALLexer&&) = delete;
    ALLexer &operator=(const ALLexer&) = delete;
    ALLexer &operator=(ALLexer&&) = delete;


    void skip_whitespace(){
        while (this->position.has_more() && char_in_alphabet(*this->position, inner::whitespace_alphabet))
        {
            ++this->position;
        }
    }

    void skip_eol(){
        while(this->position.has_more() && check_char(*this->nl.c_str()))
        {
            ++this->position;
        }
    }
    
    bool capture_symbol(std::vector<alisp::ALToken>& tokens)
    {
        
        if(char_in_alphabet(*this->position, inner::symbol_alphabet))
        {
            tokens.push_back(alisp::ALToken(symbols_map[*this->position],
                                            static_cast<size_t>(this->position.col),
                                            static_cast<size_t>(this->position.line)));
            ++this->position;
            return true;
        }
        return false;
    }

    bool capture_keyword(std::vector<alisp::ALToken>& tokens)
    {
        if (!check_char(*keyword_start.c_str())) { return false;}
        ++this->position;
        auto temp = this->position;
        while(this->position.has_more() && char_in_alphabet(*this->position, inner::keyword_alphabet))
        {
            ++this->position;
        }
        const auto word = inner::Position::str(temp, this->position);

        for (const auto& keyword : keywords)
        {
            if ( keyword == word )
            {
                tokens.push_back(alisp::ALToken(TokenType::KEYWORD, std::string(word),
                                                static_cast<size_t>(this->position.col),
                                                static_cast<size_t>(this->position.line)));
                return true;
            }
        }

        this->err.lexer_error(static_cast<size_t>(this->position.col),
                              static_cast<size_t>(this->position.line),
                              "Invalid keyword: " + std::string(word));
				
        return false;
    }

    bool capture_id(std::vector<alisp::ALToken>& tokens)
    {
        
        auto temp = this->position;
        while(this->position.has_more() && char_in_alphabet(*this->position, inner::id_alphabet))
        {
            ++this->position;
        }
        const auto word = inner::Position::str(temp, this->position);
        tokens.push_back(alisp::ALToken(TokenType::ID, std::string(word),
                                        static_cast<size_t>(this->position.col),
                                        static_cast<size_t>(this->position.line)));

        return true;
    }

    bool capture_string(std::vector<alisp::ALToken>& tokens)
    {
        
        if(*this->position != '\"') { return false; }

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
        

        const auto text = inner::Position::str(temp, this->position);
        tokens.push_back(alisp::ALToken(TokenType::STRING, std::string(text),
                                        static_cast<size_t>(this->position.col),
                                        static_cast<size_t>(this->position.line)));
        ++this->position;
        return true;
    }
	
    bool capture_num(std::vector<alisp::ALToken>& tokens)
    {
        int sign = 1;
        bool real = false;
        bool retval = true;

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
            return false;
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
        
        // if(!char_in_alphabet(*this->position, inner::whitespace_alphabet) &&
        //    !char_in_alphabet(*this->position, inner::symbol_alphabet))
        // {
        //     this->err.lexer_error(static_cast<size_t>(this->position.col),
        //                           static_cast<size_t>(this->position.line),
        //                           "Invalid number.");
        // }

        const auto res = inner::Position::str(temp, this->position);

        if (real)
        {
            float num = static_cast<float>(sign) * std::stof( std::string(res) );
            tokens.push_back(alisp::ALToken(TokenType::REAL_NUMBER, num,
                                            static_cast<size_t>(this->position.col),
                                            static_cast<size_t>(this->position.line)));
        }
        else
        {
            int num = sign * std::stoi( std::string(res) );
            tokens.push_back(alisp::ALToken(TokenType::NUMBER, num,
                                            static_cast<size_t>(this->position.col),
                                            static_cast<size_t>(this->position.line)));
        }

        return retval;
    }

    inline bool check_char(const char c)
    {
        return *this->position == c;
    }

    std::vector<alisp::ALToken> tokenize(const std::string& input)
    {
        std::vector<alisp::ALToken> tokens;
        const auto begin = input.empty() ? nullptr : &input.front();
        const auto end = begin == nullptr ? nullptr : begin + input.size();
        this->position = inner::Position(begin, end);

        while(position.has_more())
        {
            skip_whitespace();
            // skip_eol();

            if (capture_symbol(tokens)) { continue; }

            if (capture_keyword(tokens)) { continue; }

            if (capture_string(tokens)) { continue; }

            if (capture_num(tokens)) { continue; }

            if (capture_id(tokens)) { continue; }
            

            this->err.lexer_error(static_cast<size_t>(this->position.col),
                                  static_cast<size_t>(this->position.line),
                                  "Unknown lexer symbol");
					
        }
				
        return tokens;
  
    }


};


}
