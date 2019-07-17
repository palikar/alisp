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

namespace alisp
{

static const std::string STR_REG = R"((?!\")(?:[^\"\\]|\\.)*(?=\"))";
static const std::regex STR_RE{STR_REG};

static const std::string ID_REG = R"(^[\/\\\*a-zA-Z_-][\/\\\*a-zA-Z0-9_-]*)";
static const std::regex ID_RE{ID_REG};
 
static const std::string NUM_REG = R"(^[-+]?[0-9]*\.?[0-9]+(?=(?:\s|\(|\)|$)))";
static const std::regex NUM_RE{NUM_REG};

static const std::string KEYWORD_REG = R"(&optional|&rest)";
static const std::regex KEYWORD_RE{KEYWORD_REG};


enum Alphabet
{
    symbol_alphabet = 0,
    id_alphabet,
    whitespace_alphabet,
    max_alphabet,
    lengthof_alphabet = 256
};


struct Static_String
{
    template<size_t N>
    constexpr Static_String(const char (&str)[N]) noexcept
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
        //return std::string_view(data, m_size) == other;
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


template <class ErrorHandler>
class ALLexer
{
  private:
    size_t char_num = 0;
    size_t line_num = 0;

    const ErrorHandler& err;

    constexpr static std::array<Static_String, 2> create_keywords() noexcept
    {
        std::array<Static_String, 2> keywords_vec = {{
                Static_String{"&optional"},
                Static_String{"&rest"}
            }};
        return keywords_vec;
    }
    
    constexpr static Static_String keyword_start{"&"};
    constexpr static auto keywords = create_keywords();

    constexpr static std::unordered_map<char, TokenType> create_symbols() noexcept
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

    // constexpr static auto symbols_map = create_symbols();

    template<typename Array2D, typename First, typename Second>
    constexpr static void set_alphabet(Array2D &array, const First first, const Second second) noexcept
    {
        auto *first_ptr = &std::get<0>(array) + static_cast<std::size_t>(first);
        auto *second_ptr = &std::get<0>(*first_ptr) + static_cast<std::size_t>(second);
        *second_ptr = true;
    }
    
    constexpr static std::array<std::array<bool, lengthof_alphabet>, max_alphabet> build_alphabet() noexcept
    {
        std::array<std::array<bool, Alphabet::lengthof_alphabet>, Alphabet::max_alphabet> alph{};

        for (const auto& [sym, tok] : create_symbols())
        {
            set_alphabet(alph, Alphabet::symbol_alphabet, sym);
        }
        
        return alph;
    }


  public:

    
    explicit ALLexer(const ErrorHandler& err_) : err(err_)
    {
    }
    
    std::vector<alisp::ALToken> tokenize(const std::string& input)
    {
				

        std::vector<alisp::ALToken> tokens;
        const char * s = input.c_str();
        this->char_num = 0;
        this->line_num = 0;

  
        while (*s)
        {
        
            while (*s == '\n')
            {
                ++line_num;
                this->char_num = 0;
                ++s;
            }

            while (*s == ' ')
            {
                ++this->char_num;
                ++s;
            }

            if(!*s)
            {
                break;
            }
                
            if (*s == '(')
            {
                tokens.push_back(alisp::ALToken(TokenType::LEFT_BRACKET, this->char_num, this->line_num));
            }

            else if (*s == ')')
            {
                tokens.push_back(alisp::ALToken(TokenType::RIGHT_BRACKET, this->char_num, this->line_num ));
            }
					
            else if (*s == ':')
            {
                tokens.push_back(alisp::ALToken(TokenType::COLON, this->char_num, this->line_num));
            }

            else if (*s == '\'')
            {
                tokens.push_back(alisp::ALToken(TokenType::QUOTE, this->char_num, this->line_num));
            }

            else if (*s == '`')
            {
                tokens.push_back(alisp::ALToken(TokenType::BACKQUOTE, this->char_num, this->line_num));
            }

            else if (*s == '@')
            {
                tokens.push_back(alisp::ALToken(TokenType::AT, this->char_num, this->line_num));
            }

            else if (*s == '&')
            {
                std::cmatch match;
                if (std::regex_search(s, match, KEYWORD_RE))
                {
                    const std::string res = match.str(0);
                    s += res.size();
                    tokens.push_back(alisp::ALToken(TokenType::ID,
                                                    std::move(res),
                                                    this->char_num,
                                                    this->line_num));
                    continue;
                }
                else
                {
                    tokens.push_back(alisp::ALToken(TokenType::AMPER, this->char_num, this->line_num));
                }
            }
                
            else if (*s == '\"')
            {
                std::cmatch match;
                if (std::regex_search(s, match, STR_RE))
                {
                    std::string res = match.str(0);
                    s += res.size() + 1;
                    tokens.push_back(alisp::ALToken(TokenType::STRING,
                                                    std::move(res),
                                                    this->char_num,
                                                    this->line_num));
                }
                else
                {
                    this->err.lexer_error(this->char_num, this->line_num, "Invalid String");
                    exit(1);
                }
            }

            // TODO: This check is ugly, fix it!
            else if(std::isdigit(*s)
                    or ((*s=='-' or *s=='+') and std::isdigit(*(s+1)))
                    or ((*s=='.') and std::isdigit(*(s+1)))
                    or (((*(s)=='-' or *(s)=='+') and *(s+1)=='.') and std::isdigit(*(s+2))))
            {
                std::cmatch match;
                if (std::regex_search(s, match, NUM_RE)) {
                    const std::string& res = match.str(0);
                    s += res.size()-1;
                    float num = std::stof(res);
                    float intpart;
                    if (std::modf(num, &intpart) == 0.0f)
                    {
                        tokens.push_back(alisp::ALToken(TokenType::NUMBER,
                                                        static_cast<int>(num),
                                                        this->char_num,
                                                        this->line_num));
                    }
                    else
                    {
                        tokens.push_back(alisp::ALToken(TokenType::REAL_NUMBER,
                                                        num,
                                                        this->char_num,
                                                        this->line_num));
                    }
                            

                        
                }else{
                    this->err.lexer_error(this->char_num, this->line_num, "Invalid numer");
                }
            }

            else
            {
                std::cmatch match;
                if (std::regex_search(s, match, ID_RE)) {
                    std::string res = match.str(0);
                    s += res.size()-1;
                    tokens.push_back(alisp::ALToken(TokenType::ID, std::move(res)));
                }else{
                    this->err.lexer_error(this->char_num, this->line_num, "Invalid ID");
                }
            }

            ++this->char_num;
                    ++s;
        }
    
        return tokens;

        }
  
    };


}
