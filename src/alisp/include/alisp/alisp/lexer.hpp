#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>
#include <cmath>

#include "alisp/alisp/common_lexer.hpp"


namespace alisp
{


static const std::string STR_REG = R"((?!\")(?:[^\"\\]|\\.)*(?=\"))";
static const std::regex STR_RE{STR_REG};

static const std::string ID_REG = R"(^[\/\\\*a-zA-Z_-][\/\\\*a-zA-Z0-9_-]*)";
static const std::regex ID_RE{ID_REG};

static const std::string NUM_REG = R"(^[+-]?[0-9.]+(?=(?:\s|\(|\)|$)))";
static const std::regex NUM_RE{NUM_REG};
    
    
class Lexer
{
    public:
        Lexer(){};

        std::vector<alisp::Token> tokenize(const std::string& input){
            

            std::vector<alisp::Token> tokens;
            const char * s = input.c_str();

            int char_num = 0;
            int line_num = 0;
            
            while (*s)
            {
                
                while (*s == ' ')
                {
                    ++char_num;
                    ++s;
                }

                while (*s == '\n')
                {
                    ++line_num;
                    char_num = 0;
                    ++s;
                }
                
                if (*s == '(')
                {
                    tokens.push_back(alisp::Token(TokenType::LEFT_BRACKET));
                }

                else if (*s == ')')
                {
                    tokens.push_back(alisp::Token(TokenType::RIGHT_BRACKET));
                }

                else if (*s == ':')
                {
                    tokens.push_back(alisp::Token(TokenType::COLON));
                }

                else if (*s == '\'')
                {
                    tokens.push_back(alisp::Token(TokenType::QUOTE));
                }

                else if (*s == '`')
                {
                    tokens.push_back(alisp::Token(TokenType::BACKQUOTE));
                }

                else if (*s == '@')
                {
                    tokens.push_back(alisp::Token(TokenType::AT));
                }

                else if (*s == '&')
                {
                    tokens.push_back(alisp::Token(TokenType::AMPER));
                }
                
                else if (*s == '\"')
                {
                    std::cmatch match;
                    if (std::regex_search(s, match, STR_RE)) {
                        std::string res = match.str(0);
                        s += res.size() + 1;
                        tokens.push_back(alisp::Token(TokenType::STRING, std::move(res)));
                    }else{
                        std::cout << "Invalid String" << "\n";
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
                            tokens.push_back(alisp::Token(TokenType::NUMBER, static_cast<int>(num)));
                        }
                        else
                        {
                            tokens.push_back(alisp::Token(TokenType::REAL_NUMBER, num));
                        }
                            

                        
                    }else{
                        std::cout << "Invalid Number" << "\n";
                    }
                }
                
                else
                {
                    std::cmatch match;
                    if (std::regex_search(s, match, ID_RE)) {
                        std::string res = match.str(0);
                        s += res.size()-1;
                        tokens.push_back(alisp::Token(TokenType::ID, std::move(res)));
                    }else{
                        std::cout << "Invalid ID" << "\n";
                 
                    }
                }
                
                ++s;
            }
            return tokens;
            
            

        }
    };


}
