#include "alisp/alisp/alisp_common.hpp"

namespace alisp
{


#define ENUM_CASE(type) case TokenType::type :   \
    return std::string{#type};                   \
    break

std::string get_token_str(TokenType type)
{
	switch (type) {
		ENUM_CASE(ID);
		ENUM_CASE(STRING);
		ENUM_CASE(KEYWORD);
		ENUM_CASE(NUMBER);
		ENUM_CASE(REAL_NUMBER);
        
		ENUM_CASE(AT);
		ENUM_CASE(COLON);
		ENUM_CASE(BACKQUOTE);
		ENUM_CASE(QUOTE);
		ENUM_CASE(QUOTATION_MARKS);
        ENUM_CASE(HASHTAG);
		ENUM_CASE(AMPER);
		ENUM_CASE(COMMA);
        
        ENUM_CASE(LEFT_BRACE);
		ENUM_CASE(RIGHT_BRACE);
		ENUM_CASE(RIGHT_BRACKET);
		ENUM_CASE(LEFT_BRACKET);
      default : return std::string{"UNKNOWN"};
	}
}
#undef ENUM_CASE



}
