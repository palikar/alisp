#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>

#include "numler/alisp/common.hpp"



namespace alisp
{

    static const std::string STR_REG = R"((?!\")(?:[^\"\\]|\\.)*(?=\"))";
    static const std::regex STR_RE{STR_REG};
    
    class Lexer
    {
    public:
        Lexer(){};

        std::vector<alisp::Token*> tokenize(const std::string& input){

            std::vector<alisp::Token*> tokens;
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
                    tokens.push_back(new alisp::LEFT_BRACKET_TOKEN());
                }
                else if (*s == ')')
                {
                    tokens.push_back(new alisp::RIGHT_BRACKET_TOKEN());
                }
                else if (*s == ':')
                {
                    tokens.push_back(new alisp::COLON_TOKEN());
                }
                else if (*s == '\'')
                {
                    tokens.push_back(new alisp::QUOTE_TOKEN());
                }
                else if (*s == '`')
                {
                    tokens.push_back(new alisp::BACKQUOTE_TOKEN());
                }
                else if (*s == '@')
                {
                    tokens.push_back(new alisp::AT_TOKEN());
                }
                else if (*s == '&')
                {
                    tokens.push_back(new alisp::AMPER_TOKEN());
                }
                
                else if (*s == '\"')
                {
                    std::cmatch match;
                    if (std::regex_search(s, match, STR_RE)) {
                        std::string res = match.str(0);
                        s += res.size() + 1;
                        tokens.push_back(new alisp::STRING_TOKEN(std::move(res)));
                    }else{
                        std::cout << "Invalid String" << "\n";
                    }
                }
                
                else
                {
                    const char * t = s;
                    while (*t
                           && *t != ' '
                           && *t != '\n'
                           && *t != '('
                           && *t != ')')
                        ++t;
                    tokens.push_back(new alisp::ID_TOKEN(std::string{s,t}));
                    s = t;
                }
                ++s;
            }
            return tokens;
            
            

        }
    };


}
