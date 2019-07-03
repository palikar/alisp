#include "catch2/catch.hpp"

#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"

#include <string>
#include <vector>

using Catch::Matchers::Equals;



TEST_CASE("Basic Lexer Tests", "[lexer]")
{

	std::vector<alisp::ALToken> toks;
	alisp::ErrorMessanger err;
	alisp::ALLexer lex{err};
	std::string input;

	SECTION("Simple tokens"){

		SECTION("Brackets"){
			input = R"(())";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[1].getType() == alisp::TokenType::RIGHT_BRACKET);
		
			
			input = R"( () )";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[1].getType() == alisp::TokenType::RIGHT_BRACKET);

			input = R"( (
) )";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[1].getType() == alisp::TokenType::RIGHT_BRACKET);
		}

		
		SECTION("Strings"){
			input = R"("name")";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::STRING);
		

		
			input = R"( "name" )";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::STRING);
		}

		SECTION("IDs"){
			input = R"(name)";
			err.set_input(input);
			toks = lex.tokenize(input);
		
			REQUIRE(toks[0].getType() == alisp::TokenType::ID);
			input = R"( name )";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::ID);
		}

		SECTION("NUMERS"){

			input = R"(100)";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::NUMBER);

            input = R"(100.123)";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::REAL_NUMBER);

            input = R"(-100)";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::NUMBER);

            input = R"(-100.123)";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::REAL_NUMBER);

            input = R"(.123)";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::REAL_NUMBER);

            input = R"(-.123)";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::REAL_NUMBER);


        }


	}



	SECTION("Mixed tokens"){

		SECTION("Normal long input"){
			input = R"((define (arg1 arg2) "documentation" (progn (setq s 3))))";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[1].getType() == alisp::TokenType::ID);
			REQUIRE(toks[2].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[3].getType() == alisp::TokenType::ID);
			REQUIRE(toks[4].getType() == alisp::TokenType::ID);
			REQUIRE(toks[5].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[6].getType() == alisp::TokenType::STRING);
			REQUIRE(toks[7].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[8].getType() == alisp::TokenType::ID);
			REQUIRE(toks[9].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[10].getType() == alisp::TokenType::ID);
			REQUIRE(toks[11].getType() == alisp::TokenType::ID);
			REQUIRE(toks[12].getType() == alisp::TokenType::NUMBER);
			REQUIRE(toks[13].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[14].getType() == alisp::TokenType::RIGHT_BRACKET);
		}

		SECTION("Input with lots of spaces and new lines"){
			input = R"((    define   (arg1
 arg2  ) "documentation"
    (progn
               (setq s 3))
))";
            
			err.set_input(input);
			toks = lex.tokenize(input);

            REQUIRE(toks[0].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[1].getType() == alisp::TokenType::ID);
			REQUIRE(toks[2].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[3].getType() == alisp::TokenType::ID);
			REQUIRE(toks[4].getType() == alisp::TokenType::ID);
			REQUIRE(toks[5].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[6].getType() == alisp::TokenType::STRING);
			REQUIRE(toks[7].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[8].getType() == alisp::TokenType::ID);
			REQUIRE(toks[9].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[10].getType() == alisp::TokenType::ID);
			REQUIRE(toks[11].getType() == alisp::TokenType::ID);
			REQUIRE(toks[12].getType() == alisp::TokenType::NUMBER);
			REQUIRE(toks[13].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[14].getType() == alisp::TokenType::RIGHT_BRACKET);
		}


		SECTION("Input with reduced spaces") {
			
			input = R"((define(arg1 arg2) "documentation" (progn(setq s 3))))";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[1].getType() == alisp::TokenType::ID);
			REQUIRE(toks[2].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[3].getType() == alisp::TokenType::ID);
			REQUIRE(toks[4].getType() == alisp::TokenType::ID);
			REQUIRE(toks[5].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[6].getType() == alisp::TokenType::STRING);
			REQUIRE(toks[7].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[8].getType() == alisp::TokenType::ID);
			REQUIRE(toks[9].getType() == alisp::TokenType::LEFT_BRACKET);
			REQUIRE(toks[10].getType() == alisp::TokenType::ID);
			REQUIRE(toks[11].getType() == alisp::TokenType::ID);
			REQUIRE(toks[12].getType() == alisp::TokenType::NUMBER);
			REQUIRE(toks[13].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[14].getType() == alisp::TokenType::RIGHT_BRACKET);

		}
	
		
	}
	
}

TEST_CASE("More complicated Lexer Tests", "[lexer]")
{

	std::vector<alisp::ALToken> toks;
	alisp::ErrorMessanger err;
	alisp::ALLexer lex{err};
	std::string input;


	SECTION("Quoting")
	{

		input = R"((setq dsf 'nil))";
        auto output = std::vector({alisp::ALToken(alisp::TokenType::LEFT_BRACKET),
                                   alisp::ALToken(alisp::TokenType::ID),
                                   alisp::ALToken(alisp::TokenType::ID),
                                   alisp::ALToken(alisp::TokenType::QUOTE),
                                   alisp::ALToken(alisp::TokenType::ID),
                                   alisp::ALToken(alisp::TokenType::RIGHT_BRACKET)});
        err.set_input(input);
        toks = lex.tokenize(input);
        
        REQUIRE_THAT(toks, Equals(output));

    }

	SECTION("Colon")
	{
		input = R"((setq dsf :key-word-1))";
        auto output = std::vector({alisp::ALToken(alisp::TokenType::LEFT_BRACKET),
                                   alisp::ALToken(alisp::TokenType::ID),          
                                   alisp::ALToken(alisp::TokenType::ID),          
                                   alisp::ALToken(alisp::TokenType::COLON),       
                                   alisp::ALToken(alisp::TokenType::ID),          
                                   alisp::ALToken(alisp::TokenType::RIGHT_BRACKET)});
        err.set_input(input);
        toks = lex.tokenize(input);
        REQUIRE_THAT(toks, Equals(output));
    }

	SECTION("Keyword rest")
	{
        
        
        
        
        
		input = R"((setq dsf &rest))";
        auto output = std::vector({alisp::ALToken(alisp::TokenType::LEFT_BRACKET),
                                   alisp::ALToken(alisp::TokenType::ID),          
                                   alisp::ALToken(alisp::TokenType::ID),          
                                   alisp::ALToken(alisp::TokenType::ID),       
                                   alisp::ALToken(alisp::TokenType::RIGHT_BRACKET)});
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE_THAT(toks, Equals(output));
    }

	SECTION("Keyword optional")
	{
		input = R"((setq dsf &optional))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE_THAT(toks, Equals(std::vector{
                    alisp::ALToken(alisp::TokenType::LEFT_BRACKET),
                        alisp::ALToken(alisp::TokenType::ID),
                        alisp::ALToken(alisp::TokenType::ID),
                        alisp::ALToken(alisp::TokenType::ID),
                        alisp::ALToken(alisp::TokenType::RIGHT_BRACKET)}));
	}

	SECTION("Amper")
	{
		input = R"((setq dsf &nonword))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE_THAT(toks, Equals(std::vector{
                    alisp::ALToken(alisp::TokenType::LEFT_BRACKET),
                        alisp::ALToken(alisp::TokenType::ID),
                        alisp::ALToken(alisp::TokenType::ID),
                        alisp::ALToken(alisp::TokenType::AMPER),
                        alisp::ALToken(alisp::TokenType::ID),
                        alisp::ALToken(alisp::TokenType::RIGHT_BRACKET)}));
    }

	SECTION("Keyword rest")
	{
		input = R"((setq dsf @nonword))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE_THAT(toks, Equals(std::vector{
                    alisp::ALToken(alisp::TokenType::LEFT_BRACKET),
                    alisp::ALToken(alisp::TokenType::ID),
                    alisp::ALToken(alisp::TokenType::ID),
                    alisp::ALToken(alisp::TokenType::AT),
                    alisp::ALToken(alisp::TokenType::ID),
                    alisp::ALToken(alisp::TokenType::RIGHT_BRACKET)}));
	}

	
	

}


TEST_CASE("Failing checks", "[lexer]")
{

	std::vector<alisp::ALToken> toks;
	alisp::ThrowingMessanger err;
	alisp::ALLexer lex{err};
	std::string input;


	SECTION("Invalid string")
	{
		input = R"(("sadsad))";
		err.set_input(input);
        REQUIRE_THROWS(toks = lex.tokenize(input));
	}

	
	SECTION("Invalid strings")
	{
		input = R"(("sadsad))";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"((sadsad"))";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));
	}

	SECTION("Invalid numbers")
	{
		input = R"(3.1a3)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"(-2a3)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"(-2a.3)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"(2.3.3)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"(2ds.3)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"(2d)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));

		input = R"(2.2d)";
		err.set_input(input);
		REQUIRE_THROWS(toks = lex.tokenize(input));
    }

}



