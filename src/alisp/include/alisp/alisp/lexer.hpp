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

static const std::string STR_REG = R"((?!\")(?:[^\"\\]|\\.)*(?=\"))";
static const std::regex STR_RE{STR_REG};

static const std::string ID_REG = R"(^[\/\\\*a-zA-Z_-][\/\\\*a-zA-Z0-9_-]*)";
static const std::regex ID_RE{ID_REG};
 
static const std::string NUM_REG = R"(^[-+]?[0-9]*\.?[0-9]+(?=(?:\s|\(|\)|$)))";
static const std::regex NUM_RE{NUM_REG};

static const std::string KEYWORD_REG = R"(&optional|&rest)";
static const std::regex KEYWORD_RE{KEYWORD_REG};

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
}


template <class ErrorHandler>
class ALLexer
{
  private:
    size_t char_num = 0;
    size_t line_num = 0;
    const char *input;

    const ErrorHandler& err;

    constexpr static std::array<LiteString, 2> create_keywords() noexcept
    {
        std::array<LiteString, 2> keywords_vec = {{
                LiteString{"&optional"},
                LiteString{"&rest"}
            }};
        return keywords_vec;
    }
    
    constexpr static LiteString keyword_start{"&"};
    constexpr static auto keywords = create_keywords();

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

    constexpr bool char_in_alphabet(char c, detail::Alphabet a) const noexcept { 
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

  public:

    
    explicit ALLexer(const ErrorHandler& err_) : err(err_)
    {
    }

    ALLexer(const ALLexer&) = delete;
    ALLexer(const ALLexer&&) = delete;


    void skip_whitespace(){
        while (char_in_alphabet(*input, inner::whitespace_alphabet))
        {
            ++this->char_num;
            ++input;
        }
    }

    
    bool capture_symbol(std::vector<alisp::ALToken>& tokens)
    {
        
        if(char_in_alphabet(*input, inner::symbol_alphabet))
        {
            tokens.push_back(alisp::ALToken(symbols_map[*input];, this->char_num, this->line_num));
            return true;
        }
        return false;
    }

    bool capture_keyword(std::vector<alisp::ALToken>& tokens)
    {
        char *temp = input;
        while(char_in_alphabet(*input, inner::keyword_alphabet))
        {
            this->char_num++;
            ++input;
        }
        std::string word{temp, input};

        for (const auto& key : keywords())
        {
            if (key == word)
            {
                tokens.push_back(alisp::ALToken(TokenType::ID,
                                                std::move(word),
                                                this->char_num,
                                                this->line_num));
                return true;
            }
        }
        return false;
    }

    bool capture_id(std::vector<alisp::ALToken>& tokens)
    {
        char *temp = input;
        while(char_in_alphabet(*input, inner::keyword_alphabet))
        {
            this->char_num++;
            ++input;
        }
        std::string word{temp, input};
        tokens.push_back(alisp::ALToken(TokenType::ID,
                                        std::move(word),
                                        this->char_num,
                                        this->line_num));
        return true;
    }

    

    inline bool check_char(const char c)
    {
        return *input == c;
    }

    
    std::vector<alisp::ALToken> tokenize(const std::string& input)
    {
				

        std::vector<alisp::ALToken> tokens;
        this->input = input.c_str();
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
