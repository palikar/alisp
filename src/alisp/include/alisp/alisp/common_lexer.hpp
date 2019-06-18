#pragma once

#include <string>
#include <sstream>
#include <variant>


namespace alisp
{

enum class TokenType
{
    ID,
    STRING,
    NUMBER,
    REAL_NUMBER,
    
    LEFT_BRACE,
    RIGHT_BRACE,

    LEFT_BRACKET,
    RIGHT_BRACKET,

    AMPER,
    QUOTATION_MARKS,
    QUOTE,
    BACKQUOTE,
    AT,
    COLON
};

std::string get_token_str(TokenType type);


class ALToken;
inline std::ostream& operator<<(std::ostream& os, const ALToken& x);

class ALToken
{
  public:


    ALToken(TokenType type_, size_t char_n=0, size_t line=0)
        : type(type_), content(),
          line_num(line), char_num(char_n)
    {};

    template<typename T>
    ALToken(TokenType type_, T content_, size_t char_n=0, size_t line=0)
        : type(type_),
          line_num(line), char_num(char_n)

    {
        content = std::move(content_);
    }


    template<typename T>
    T getContentAs() const
    {
        return std::get<T>(this->content);
    }
    
    std::string str() const
    {
        std::ostringstream os;
        os << *this;
        return os.str();
    };

    TokenType getType() const
    {
        return type;
    };

    
    size_t getLine() const
    {
        return this->line_num;
    };

    size_t getChar() const
    {
        return this->char_num;
    };


    friend inline std::ostream& operator<<(std::ostream& os, const ALToken& x);

    
  private:
    TokenType type;
    std::variant<int, float, std::string> content;
    size_t line_num;
    size_t char_num;
    
};



inline std::ostream& operator<<(std::ostream& os, const ALToken& x)
{
    os << "<Token (";
    os << get_token_str(x.getType());
    os << ") ";

    if (const auto val1 = std::get_if<0>(&x.content)) 
        os << *val1;

    else if (const auto val2 = std::get_if<1>(&x.content)) 
        os << *val2;

    else if (const auto val3 = std::get_if<2>(&x.content)) 
        os << *val3;
    
    os << ">";
    return os;
}


}  // namespace alisp
