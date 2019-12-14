#include "catch2/catch.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include <string>
#include <vector>

using Catch::Matchers::Equals;



TEST_CASE("Basic Parser Tests [1]", "parser")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);


    std::string input{"(println 12)"};
    auto res = pars.parse(&input, "__TEST__");

    REQUIRE ( std::size(res) == 1 );
    REQUIRE ( res[0]->is_list() );
    REQUIRE ( res[0]->i(0)->is_sym() );
    REQUIRE ( res[0]->i(1)->is_int() );

}


TEST_CASE("Basic Parser Tests [2]", "parser")
{
    alisp::env::Environment env;
    alisp::eval::Evaluator eval(env);
    alisp::parser::ALParser<alisp::env::Environment> pars(env);


    std::string input{"12"};
    auto res = pars.parse(&input, "__TEST__");

    REQUIRE ( std::size(res) == 1 );
    REQUIRE ( res[0]->is_int() );
    

}



