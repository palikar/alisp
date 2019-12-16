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


TEST_CASE("Evaluator Test [simple]", "[eval]")
{
    using namespace alisp;

    env::Environment env;
    eval::Evaluator eval(env);
    parser::ALParser<alisp::env::Environment> pars(env);

    SECTION ("int") {
        std::string input{"42"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->is_int() );
        CHECK ( res->to_int() == 42 );
    }
    
    SECTION ("double") {
        std::string input{"42.32"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->is_real() );
        CHECK ( res->to_real() == 42.32_a );
    }

    SECTION ("string") {
        
        std::string input{"\"string\""};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->is_string() );
        CHECK ( res->to_string().compare("string") == 0 );
    
    }

    SECTION ("sym") {
        std::string input{"nil"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);
        
        CHECK ( res->is_sym() );
    }    

}


TEST_CASE("Evaluator Test [language]", "[eval]")
{
    using namespace alisp;

    env::Environment env;
    eval::Evaluator eval(env);
    parser::ALParser<alisp::env::Environment> pars(env);

    SECTION ( "defun" ) {
        
        std::string input{"(defun fun (a) \'a)"};
        eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( env.find(make_symbol("fun"))->check_function_flag() );
        
    }

    SECTION ( "defmacro" ) {
        
        
        std::string input{"(defmacro fun (a) \'a)"};
        eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( env.find(make_symbol("fun"))->check_macro_flag() );
        
    
    }

    SECTION ( "defvar" ) {

        std::string input{"(defvar a 42)"};
        eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( env.find(make_symbol("a"))->is_int() );
        
    }

    SECTION ( "setq" ) {

        std::string input{"(defvar a 42)"};
        eval.eval(pars.parse(&input, "__TEST__")[0]);
        
        input = "(setq a 43)";
        eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( env.find(make_symbol("a"))->is_int() );
        CHECK ( env.find(make_symbol("a"))->to_int() == 43 );
        
    }

    SECTION ( "set" ) {

        std::string input{"(defvar a 42)"};
        eval.eval(pars.parse(&input, "__TEST__")[0]);
        
        input = "(set \'a 44)";
        eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( env.find(make_symbol("a"))->is_int() );
        CHECK ( env.find(make_symbol("a"))->to_int() == 44 );
    
    }

    SECTION ( "quote" ) {
        std::string input{"'sym"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->is_sym() );
    }

    SECTION ( "function" ) {
        std::string input{"(function a)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->is_sym() );
    }

    SECTION ( "lambda" ) {
        
        std::string input{"(lambda (a) \'a)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->check_function_flag() );
            
    }

    SECTION ( "if[1]" ) {
        std::string input{"(if 'nil 42 32)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 32 );
    }

    SECTION ( "if[2]" ) {
        std::string input{"(if 't 42 32)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 42 );
    }

    SECTION ( "while" ) {}

    SECTION ( "dolist" ) {}

    SECTION ( "cond" ) {}

    SECTION ( "unless" ) {
        
        std::string input{"(unless 'nil 42 32)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 32 );    
    }

    SECTION ( "when" ) {
        
        std::string input{"(when 't 42 32)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 32 );    
    }

    SECTION ( "progn" ) {
        std::string input{"(progn 42 32)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 32 );
    }

    SECTION ( "let*" ) {
        std::string input{"(let* ((a 42)) a)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 42 );
    
    }

    SECTION ( "let" ) {
        std::string input{"(let ((a 42)) a)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 42 );
    }

    SECTION ( "funcall" ) {}

    SECTION ( "signal" ) {}

    SECTION ( "exit" ) {}
    


}


TEST_CASE("Evaluator Test [math]", "[eval]")
{
    using namespace alisp;

    env::Environment env;
    eval::Evaluator eval(env);
    parser::ALParser<alisp::env::Environment> pars(env);

    SECTION ( "+" ) {
        std::string input{"(+ 10 10)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 20 );
        
    }
    SECTION ( "-" ) {
    
        std::string input{"(- 10 5)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 5 );
        
    }
    SECTION ( "/" ) {
    
        std::string input{"(/ 10 2)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 5 );
        
    }
    SECTION ( "*" ) {
        
        std::string input{"(* 10 10)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( res->to_int() == 100 );    
    }

    SECTION ( ">" ) {
        
        std::string input{"(> 10 11)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( is_falsy(res) );
        
    
    }
    SECTION ( ">=" ) {
    
        
        std::string input{"(>= 10 11)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( is_falsy(res) );    
    
    }
    SECTION ( "<" ) {
        std::string input{"(< 10 11)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( is_truthy(res) );
    }
    SECTION ( "<=" ) {
        std::string input{"(<= 10 11)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( is_truthy(res) );
    }
    SECTION ( "==" ) {
        std::string input{"(== 10 10)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( is_truthy(res) );
    }
    SECTION ( "!=" ) {
        std::string input{"(!= 11 11)"};
        auto res = eval.eval(pars.parse(&input, "__TEST__")[0]);

        CHECK ( is_falsy(res) );
    }
    
}



TEST_CASE("Evaluator Test [logic]", "[eval]")
{
    using namespace alisp;

    env::Environment env;
    eval::Evaluator eval(env);
    parser::ALParser<alisp::env::Environment> pars(env);

    SECTION ( "or" ) {}
    SECTION ( "and" ) {}
    SECTION ( "not" ) {}    
    
}



TEST_CASE("Evaluator Test [predicates]", "[eval]")
{
    using namespace alisp;

    env::Environment env;
    eval::Evaluator eval(env);
    parser::ALParser<alisp::env::Environment> pars(env);


    SECTION ( "pfunction" ) {}
    SECTION ( "psym" ) {}
    SECTION ( "plist" ) {}
    SECTION ( "pint" ) {}
    SECTION ( "preal" ) {}
    SECTION ( "pstring" ) {}
    
}



TEST_CASE("Evaluator Test [lists]", "[eval]")
{
    using namespace alisp;

    env::Environment env;
    eval::Evaluator eval(env);
    parser::ALParser<alisp::env::Environment> pars(env);

    SECTION ( "mapc" ) {}
    
}
