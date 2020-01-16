#include "catch2/catch.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include <string>
#include <vector>
#include <iostream>

using Catch::Matchers::Equals;
using namespace Catch::literals;



TEST_CASE("Engine Test [simple]", "[engine]")
{
    using namespace alisp;

    LanguageEngine engine();
    
    SECTION ("basic") {
        std::string input{"(defvar var 42) (setq var (+ var 1))"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }
}


TEST_CASE("Engine Test [imports]", "[engine]")
{
    using namespace alisp;

    LanguageEngine engine();
    
    SECTION ("basic") {
        std::string input{"(import 'fileio)"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }

    SECTION ("all") {
        std::string input{"(import 'fileio :all)"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }

    SECTION ("as") {
        std::string input{"(import 'fileio :all :as 'files)"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }
    
}


TEST_CASE("Engine Test [settings]", "[engine]")
{
    using namespace alisp;

    LanguageEngine engine({EngineSettings::PARSER_DEBUG, EngineSettings::EVAL_DEBUG});
    
    SECTION ("basic") {
        std::string input{"(println \"Hello world\")"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }    
}


TEST_CASE("Engine Test [args]", "[engine]")
{
    using namespace alisp;

    LanguageEngine engine({}, {"-one -two"});
    
    SECTION ("basic") {
        std::string input{"(dump --argv-- )"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }    
}
