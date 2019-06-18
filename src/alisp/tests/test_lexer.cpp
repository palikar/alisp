#include "catch2/catch.hpp"

#include "alisp/alisp/common_lexer.hpp"
#include "alisp/alisp/lexer.hpp"

#include <string>
#include <vector>

using Catch::Matchers::Equals;



TEST_CASE("Basic Lexer Tests", "[lexer]")
{

	std::vector<alisp::TokenType> toks;
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
			REQUIRE(toks[0].getType() == alisp::TokenType::STRING);
		
			input = R"( name )";
			err.set_input(input);
			toks = lex.tokenize(input);
			REQUIRE(toks[0].getType() == alisp::TokenType::STRING);
		}

		SECTION("NUMERS"){
			INPUT = R"(100)";
			ERR.SET_INPUT(INPUT);
			TOKS = LEX.TOKENIZE(INPUT);
			REQUIRE(TOKS[0].GETTYPE() == ALISP::TOKENTYPE::NUMBER);
		

			INPUT = R"(100.123)";
			ERR.SET_INPUT(INPUT);
			TOKS = LEX.TOKENIZE(INPUT);
			REQUIRE(TOKS[0].GETTYPE() == ALISP::TOKENTYPE::REAL_NUMBER);
		

			INPUT = R"(-100)";
			ERR.SET_INPUT(INPUT);
			TOKS = LEX.TOKENIZE(INPUT);
			REQUIRE(TOKS[0].GETTYPE() == ALISP::TOKENTYPE::NUMBER);


			INPUT = R"(-100.123)";
			ERR.SET_INPUT(INPUT);
			TOKS = LEX.TOKENIZE(INPUT);
			REQUIRE(TOKS[0].GETTYPE() == ALISP::TOKENTYPE::REAL_NUMBER);


			INPUT = R"(.123)";
			ERR.SET_INPUT(INPUT);
			TOKS = LEX.TOKENIZE(INPUT);
			REQUIRE(TOKS[0].GETTYPE() == ALISP::TOKENTYPE::REAL_NUMBER);

			INPUT = R"(-.123)";
			ERR.SET_INPUT(INPUT);
			TOKS = LEX.TOKENIZE(INPUT);
			REQUIRE(TOKS[0].GETTYPE() == ALISP::TOKENTYPE::REAL_NUMBER);
			
		}


	}



	SECTION("Mixed tokens"){

		SECTION("Normal long input"){
			input = R"(define (arg1 arg2) "documentation" (progn (setq s 3)))";
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
			REQUIRE(toks[11].getType() == alisp::TokenType::NUMER);
			REQUIRE(toks[12].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[13].getType() == alisp::TokenType::RIGHT_BRACKET);
		}

		SECTION("Input with lots of spaces and new lines"){
			input = R"(   define (arg1    
arg2)    "documentation" (progn

 (  setq s 3)  ))";
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
			REQUIRE(toks[11].getType() == alisp::TokenType::NUMER);
			REQUIRE(toks[12].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[13].getType() == alisp::TokenType::RIGHT_BRACKET);
		}


		SECTION("Input with reduced spaces") {
			
			input = R"(define(arg1 arg2) "documentation" (progn(setq s 3)))";
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
			REQUIRE(toks[11].getType() == alisp::TokenType::NUMER);
			REQUIRE(toks[12].getType() == alisp::TokenType::RIGHT_BRACKET);
			REQUIRE(toks[13].getType() == alisp::TokenType::RIGHT_BRACKET);

		}
	
		
	}
	
}

TEST_CASE("More complicated Lexer Tests", "[lexer]")
{

	std::vector<alisp::TokenType> toks;
	alisp::ErrorMessanger err;
	alisp::ALLexer lex{err};
	std::string input;


	SECTION("Quoting")
	{
		input = R"((setq dsf 'nil))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE(toks, Equals({alisp::TokenType::LEFT_BRACKET,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_QUOTE,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_BRACKET}));	
	}

	SECTION("Colon")
	{
		input = R"((setq dsf :key-word-1))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE(toks, Equals({alisp::TokenType::LEFT_BRACKET,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_COLON,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_BRACKET}));
	}

	SECTION("Keyword rest")
	{
		input = R"((setq dsf &rest))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE(toks, Equals({alisp::TokenType::LEFT_BRACKET,
													alisp::TokenType::RIGHT_ID,
												alisp::TokenType::RIGHT_ID,
												alisp::TokenType::RIGHT_ID,
												alisp::TokenType::RIGHT_BRACKET}));
	}

	SECTION("Keyword optional")
	{
		input = R"((setq dsf &optional))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE(toks, Equals({alisp::TokenType::LEFT_BRACKET,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_BRACKET}));
	}

	SECTION("Amper")
	{
		input = R"((setq dsf &nonword))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE(toks, Equals({alisp::TokenType::LEFT_BRACKET,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_AMPER,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_BRACKET}));
	}

	SECTION("Keyword rest")
	{
		input = R"((setq dsf @nonword))";
		err.set_input(input);
		toks = lex.tokenize(input);
		REQUIRE(toks, Equals({alisp::TokenType::LEFT_BRACKET,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_AT,
													alisp::TokenType::RIGHT_ID,
													alisp::TokenType::RIGHT_BRACKET}));
	}

	
	

}


TEST_CASE("Failing checks", "[lexer]")
{

	std::vector<alisp::TokenType> toks;
	alisp::ThrowingMessanger err;
	alisp::ALLexer lex{err};
	std::string input;


	SECTION("Invalid string")
	{
		input = R"(("sadsad))";
		err.set_input(input);
		toks = lex.tokenize(input);
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



