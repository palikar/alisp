#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <regex>
#include <utility>
#include <locale>

#include "numler/alisp/common.hpp"



namespace alisp
{

    static const std::string STR_REG = R"((?!\")(?:[^\"\\]|\\.)*(?=\"))";
    static const std::regex STR_RE{STR_REG};

    static const std::string ID_REG = R"(^[\/\\\*a-zA-Z_-][\/\\\*a-zA-Z0-9_-]*)";
    static const std::regex ID_RE{ID_REG};

    static const std::string NUM_REG = R"(^[0-9.]+(?=(?:\s|\(|\)|$)))";
    static const std::regex NUM_RE{NUM_REG};
    
    
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

                else if(std::isdigit(*s))
                {
                    std::cmatch match;
                    if (std::regex_search(s, match, NUM_RE)) {
                        std::string res = match.str(0);
                        s += res.size()-1;
                        tokens.push_back(new alisp::NUMBER_TOKEN(std::stoi(res)));
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
                        tokens.push_back(new alisp::ID_TOKEN(std::move(res)));
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
