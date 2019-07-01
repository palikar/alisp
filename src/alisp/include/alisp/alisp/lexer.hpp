#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <cmath>

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

	

template <class ErrorHandler>
class ALLexer
{
  private:
    size_t char_num = 0;
    size_t line_num = 0;

    const ErrorHandler& err;
    
  public:

    explicit ALLexer(const ErrorHandler& err_) : err(err_)
    {}
    
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

            else if(std::isdigit(*s) or ((*s=='-' or *s=='+') and std::isdigit(*(s+1))))
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
