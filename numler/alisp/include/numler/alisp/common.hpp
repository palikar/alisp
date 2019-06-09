#pragma once

#include <string>
#include <sstream>


namespace alisp
{

enum class TokenType
{
    ID,
    STRING,
    NUMBER,
    
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

#define ENUM_CASE(type) case TokenType::type :   \
    return std::string{#type};                   \
    break

std::string get_token_str(TokenType type)
{
    switch (type) {
        ENUM_CASE(ID);
        ENUM_CASE(STRING);
        ENUM_CASE(NUMBER);

        ENUM_CASE(AT);
        ENUM_CASE(COLON);
        ENUM_CASE(BACKQUOTE);
        ENUM_CASE(QUOTE);
        ENUM_CASE(QUOTATION_MARKS);
        
        ENUM_CASE(LEFT_BRACE);
        ENUM_CASE(RIGHT_BRACE);
        ENUM_CASE(RIGHT_BRACKET);
        ENUM_CASE(LEFT_BRACKET);
        ENUM_CASE(AMPER);
      default : return std::string{"UNKNOWN"};
    }
}

#undef ENUM_CASE



class Token
{
  public:
    Token(){};
    
    virtual std::string str() const = 0;
    virtual TokenType getType() const = 0;
    virtual ~Token(){};
};

template<TokenType t>
class SimpleToken : public Token
{
  private:
    TokenType type = t;
  public:
    SimpleToken(){};

    TokenType getType() const override
    {
        return this->type;
    }

    
    std::string str() const override 
    {        
        std::ostringstream os;
        os << *this;
        return os.str();
    }
    virtual ~SimpleToken(){};
};

template<TokenType TYPE, typename T>
class ContentToken : public SimpleToken<TYPE>
{
  private:
    T content;
  public:
    ContentToken(const T &content_) : content(content_) {};

    std::string str() const override
    {
        std::ostringstream os;
        os << *this;
        return os.str();
    }

    T getContent() const
    {
        return this->content;
    }

    template<typename Y>
    Y getContentAs() const
    {
        return static_cast<Y*>(this->content);
    }

    virtual ~ContentToken(){};
};

inline std::ostream& operator<<(std::ostream& os, const Token&)
{
    os << "<Token (";
    os << ")>";
    return os;
}

template<TokenType t>
inline std::ostream& operator<<(std::ostream& os, const SimpleToken<t>& x)
{
    os << "<Token (";
    os << get_token_str(x.getType());
    os << ")>";
    return os;
}

template<TokenType TYPE, typename T>
inline std::ostream& operator<<(std::ostream& os, const ContentToken<TYPE, T>& x)
{
    os << "<Token (";
    os << get_token_str(x.getType());
    os << " : ";
    os << x.getContent();
    os << " )>";
    return os;
}

using LEFT_BRACE_TOKEN = SimpleToken<TokenType::LEFT_BRACE>;
using RIGHT_BRACE_TOKEN = SimpleToken<TokenType::RIGHT_BRACE>;

using LEFT_BRACKET_TOKEN = SimpleToken<TokenType::LEFT_BRACKET>;
using RIGHT_BRACKET_TOKEN = SimpleToken<TokenType::RIGHT_BRACKET>;

using STRING_TOKEN = ContentToken<TokenType::STRING, std::string>;
using ID_TOKEN = ContentToken<TokenType::ID, std::string>;
using NUMBER_TOKEN = ContentToken<TokenType::NUMBER, int>;

using QUOTATION_MARKS_TOKEN = SimpleToken<TokenType::QUOTATION_MARKS>;
using QUOTE_TOKEN = SimpleToken<TokenType::QUOTE>;
using AT_TOKEN = SimpleToken<TokenType::AT>;
using BACKQUOTE_TOKEN = SimpleToken<TokenType::BACKQUOTE>;
using COLON_TOKEN = SimpleToken<TokenType::COLON>;
using AMPER_TOKEN = SimpleToken<TokenType::AMPER>;


}  // namespace alisp
