#include <vector>
#include <string>

//#include "absl/strings/str_split.h"

#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"

namespace alisp
{


static const std::string STR_REG = R"((?!\")(?:[^\"\\]|\\.)*(?=\"))";
static const std::regex STR_RE{STR_REG};

static const std::string ID_REG = R"(^[\/\\\*a-zA-Z_-][\/\\\*a-zA-Z0-9_-]*)";
static const std::regex ID_RE{ID_REG};

static const std::string NUM_REG = R"(^[+-]?[0-9.]+(?=(?:\s|\(|\)|$)))";
static const std::regex NUM_RE{NUM_REG};

static const std::string KEYWORD_REG = R"(&optional|&rest)";
static const std::regex KEYWORD_RE{KEYWORD_REG};


ALLexer::ALLexer(const ErrorMessanger& err_) : err(err_)
{
}

std::vector<alisp::ALToken> ALLexer::tokenize(const std::string& input){

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
            tokens.push_back(alisp::ALToken(TokenType::LEFT_BRACKET));
        }

        else if (*s == ')')
        {
            tokens.push_back(alisp::ALToken(TokenType::RIGHT_BRACKET));
        }

        else if (*s == ':')
        {
            tokens.push_back(alisp::ALToken(TokenType::COLON));
        }

        else if (*s == '\'')
        {
            tokens.push_back(alisp::ALToken(TokenType::QUOTE));
        }

        else if (*s == '`')
        {
            tokens.push_back(alisp::ALToken(TokenType::BACKQUOTE));
        }

        else if (*s == '@')
        {
            tokens.push_back(alisp::ALToken(TokenType::AT));
        }

        else if (*s == '&')
        {
            
            std::cmatch match;
            if (std::regex_search(s, match, KEYWORD_RE))
            {
                const std::string res = match.str(0);
                s += res.size();
                tokens.push_back(alisp::ALToken(TokenType::ID, std::move(res)));
                continue;
            }
            else
            {
                tokens.push_back(alisp::ALToken(TokenType::AMPER));
            }
        }
                
        else if (*s == '\"')
        {
            std::cmatch match;
            if (std::regex_search(s, match, STR_RE))
            {
                std::string res = match.str(0);
                s += res.size() + 1;
                tokens.push_back(alisp::ALToken(TokenType::STRING, std::move(res)));
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
                    tokens.push_back(alisp::ALToken(TokenType::NUMBER, static_cast<int>(num)));
                }
                else
                {
                    tokens.push_back(alisp::ALToken(TokenType::REAL_NUMBER, num));
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
                tokens.push_back(alisp::ALToken(TokenType::ID, std::move(res)));
            }else{

                // size_t i = this->char_num < 10 ? 0 : this->char_num - 10;
                // std::cout << "Invalid ID at (line:" << this->line_num << ", char:" << this->char_num << ")." <<  "\n";
                // std::cout << "======>  ";
                // std::cout << lines[this->line_num].substr(i);
                // std::cout << "\n";
                // exit(1);
                
            }
        }

        ++this->char_num;
        ++s;
    }
    return tokens;
}


}
