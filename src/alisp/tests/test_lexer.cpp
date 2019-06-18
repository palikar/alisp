#define CATCH_CONFIG_MAIN

#include "catch2/catch.hpp"

#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"

TEST_CASE("Basic test", "[equality]")
{
    alisp::ErrorMessanger err;
    alisp::ALLexer lex{err};
    
    
    
	REQUIRE( 3 == 3 );
}
