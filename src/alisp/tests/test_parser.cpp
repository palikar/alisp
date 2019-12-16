#include "catch2/catch.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include <string>
#include <vector>

using Catch::Matchers::Equals;
using namespace Catch::literals;


TEST_CASE("Parser Test [simple list]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);


    std::string input{"(println 12)"};
    auto res = pars.parse(&input, "__TEST__");

    CHECK ( std::size(res) == 1 );
    CHECK ( res[0]->is_list() );
    CHECK ( res[0]->i(0)->is_sym() );
    CHECK ( res[0]->i(1)->is_int() );

}

TEST_CASE("Parser Test [int literals]", "[parser]")
{
    SECTION ("int [1]") {
        
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);


        std::string input{"12"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 12 );
    }

    SECTION ("int [2]") {
        
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);


        std::string input{"-12"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == -12 );

    }
}

TEST_CASE("Parser Test [double literals]", "[parser]")
{

    SECTION ("double [1]") {
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);
        
        std::string input{"-1.12"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_real() );
        CHECK ( res[0]->to_real() == -1.12_a );
    }

    SECTION ("double [2]") {
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);


        std::string input{"-1.12e1"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_real() );
        CHECK ( res[0]->to_real() == -1.12e1_a );
    }

    SECTION ("double [3]") {
        
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);


        std::string input{"1.12e-1"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_real() );
        CHECK ( res[0]->to_real() == 1.12e-1_a );
    }

    SECTION ("double [4]") {
        
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);


        std::string input{"1.12e1"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_real() );
        CHECK ( res[0]->to_real() == 1.12e1_a );
    }

    SECTION ("double [5]") {
        
        alisp::env::Environment env;
        alisp::eval::Evaluator eval(env);
        alisp::parser::ALParser<alisp::env::Environment> pars(env);


        std::string input{"12.12"};
        auto res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_real() );
        CHECK ( res[0]->to_real() == 12.12 );

    }
    
}

TEST_CASE("Parser Test [number literals]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");


    SECTION("Binary [0]"){

        input = "#b0010";
        res = pars.parse(&input, "__TEST__");


        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 2 );

    }

    
    SECTION("Binary [1]"){
        input = "#b1010";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 10 );
    }

    SECTION("Binary [2]"){
        input = "#b11010";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 26 );
    }

    SECTION("Octal [1]"){

        input = "#o007";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 7 );

    }


    SECTION("Hex [1]"){

        input = "#x00F";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 15 );

    }
    
}

TEST_CASE("Parser Test [char literas]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");
    
    SECTION ("char [1]") {
        input = "?A";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 65 );        
    }

    SECTION ("char [2]") {
        input = "?a";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 97 );        
    }
    
    SECTION ("char [3]") {
        input = "?\n";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 10 );
    }

    SECTION ("char [4]") {
        input = "\\";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 92 );
    }

    SECTION ("char [5]") {
        input = "\'";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == 39 );
    }

    SECTION ("char [6]") {
        input = "\r";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_int() );
        CHECK ( res[0]->to_int() == static_cast<static_cast<ALObject::int_type>>('\r') );
    }



}

TEST_CASE("Parser Test [string literas]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");
    
    SECTION ("strings [1]") {
        input = R"raw("string")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("string") == 0 );
    }

    SECTION ("strings [2]") {
        input = R"raw("At:\u0040")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("At:@") == 0 );
    }

    SECTION ("strings [3]") {
        input = R"raw("Hmm:\077")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:?") == 0 );
    }

    SECTION ("strings [4]") {
        input = R"raw("Hmm:\x4F")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:O") == 0 );
    }

    SECTION ("strings [5]") {
        input = R"raw("Hmm:\n")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\n") == 0 );
    }
    
    SECTION ("strings [6]") {
        input = R"raw("Hmm:\t")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\t") == 0 );
    }
    
    SECTION ("strings [7]") {
        input = R"raw("Hmm:\\")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\\") == 0 );
    }

    SECTION ("strings [8]") {
        input = R"raw("Hmm:\"")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\"") == 0 );
    }

    SECTION ("strings [9]") {
        input = R"raw("Hmm:\u0400")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\u0400") == 0 );
    }

    SECTION ("strings [10]") {
        input = R"raw("Hmm:\u1400")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\u1400") == 0 );
    }

    SECTION ("strings [11]") {
        input = R"raw("Hmm:\u1040")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\u1040") == 0 );
    }

    SECTION ("strings [12]") {
        input = R"raw("Hmm:\uD802")raw";
        CHECK_THROWS ( res = pars.parse(&input, "__TEST__") );
    }

    SECTION ("strings [13]") {
        input = R"raw("Hmm:\U1001F1")raw";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_string() );
        CHECK ( res[0]->to_string().compare("Hmm:\U1001B1") == 0 );
    }
    
}

TEST_CASE("Parser Test [symbols]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");
    
    SECTION ("symbol [1]") {
        std::string sym{"sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [2]") {
        std::string sym{"sym++sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [3]") {
        std::string sym{"--sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [4]") {
        std::string sym{"_sym_12_sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [5]") {
        std::string sym{"sym?sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [6]") {
        std::string sym{"sym@sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [7]") {
        std::string sym{"sym&sym"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }
    
    SECTION ("symbol [8]") {
        std::string sym{"<symsym>"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [9]") {
        std::string sym{"++"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

    SECTION ("symbol [10]") {
        std::string sym{"-"};
        input = sym;
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( res[0]->to_string().compare(sym) == 0 );
    }

}

TEST_CASE("Parser Test [quote]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");
    
    SECTION ("quote [1]") {

        input = "\'(sym sym)";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );
        CHECK ( res[0]->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->is_list() );
    }

    SECTION ("quote [2]") {

        input = "\'sym";
        res = pars.parse(&input, "__TEST__");
        
        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );
        CHECK ( res[0]->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->is_sym() );
        CHECK ( res[0]->i(1)->to_string().compare("sym") == 0 );
    }

    SECTION ("quote [3]") {

        input = "\'(1 2 3)";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );
        CHECK ( res[0]->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->is_list() );
        CHECK ( res[0]->i(1)->length() == 3 );

    }
}

TEST_CASE("Parser Test [list]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");

    SECTION ("list [1]") {

        input = "(sym (nil 12 (inner (more innter (most inner)))))";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );
        
        CHECK ( res[0]->i(1)->is_list() );
        CHECK ( res[0]->i(1)->length() == 3 );
        
        CHECK ( res[0]->i(1)->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->i(1)->is_int() );
        CHECK ( res[0]->i(1)->i(2)->is_list() );

        CHECK ( res[0]->i(1)->i(2)->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->i(2)->i(1)->is_list() );

        CHECK ( res[0]->i(1)->i(2)->i(1)->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->i(2)->i(1)->i(1)->is_sym() );
        CHECK ( res[0]->i(1)->i(2)->i(1)->i(2)->is_list() );
    }

    
    SECTION ("list [2]") {

        input = "()";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_sym() );
        CHECK ( alisp::is_falsy(res[0]) );
    }

}

TEST_CASE("Parser Test [comma, at, backqoute]", "[parser]")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);

    std::string input{"#b0010"};
    auto res = pars.parse(&input, "__TEST__");

    SECTION ("comma [1]") {

        input = ",(a b v)";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );

        CHECK ( res[0]->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->is_list() );

        CHECK ( res[0]->i(0)->to_string().compare(",") == 0 );
        CHECK ( res[0]->i(1)->is_list() );
    }

    SECTION ("comma [2]") {

        input = ",@(a b v)";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );

        CHECK ( res[0]->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->is_list() );

        CHECK ( res[0]->i(0)->to_string().compare(",@") == 0 );
        CHECK ( res[0]->i(1)->is_list() );
    }

    
    SECTION ("comma [3]") {

        input = "`(a b v)";
        res = pars.parse(&input, "__TEST__");

        CHECK ( std::size(res) == 1 );
        CHECK ( res[0]->is_list() );
        CHECK ( res[0]->length() == 2 );

        CHECK ( res[0]->i(0)->is_sym() );
        CHECK ( res[0]->i(1)->is_list() );

        CHECK ( res[0]->i(0)->to_string().compare("`") == 0 );
        CHECK ( res[0]->i(1)->is_list() );
    }

    
}
