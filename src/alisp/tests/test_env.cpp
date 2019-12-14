#include "catch2/catch.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include <string>
#include <vector>

using Catch::Matchers::Equals;
using namespace Catch::literals;



TEST_CASE("Environment Test [define_var]", "[env]")
{

    using namespace alisp;
    env::Environment env;
    eval::Evaluator eval(env);

    SECTION ("int") {
        
        env.define_variable(make_symbol("new-int-var"), make_int(5));

        CHECK ( env.find(make_symbol("new-int-var"))->is_int() );
        CHECK ( env.find(make_symbol("new-int-var"))->to_int() == 5 );
    }


    SECTION ("double") {
        
        env.define_variable(make_symbol("new-double-var"), make_double(5.5));

        CHECK ( env.find(make_symbol("new-double-var"))->is_real() );
        CHECK ( env.find(make_symbol("new-double-var"))->to_real() == 5.5_a );
    }

    SECTION ("string") {
        
        env.define_variable(make_symbol("new-double-var"), make_string("string"));

        CHECK ( env.find(make_symbol("new-double-var"))->is_string() );
        CHECK ( env.find(make_symbol("new-double-var"))->to_string().compare("string") == 0 );
    }
    

}
