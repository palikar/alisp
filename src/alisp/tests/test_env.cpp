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

    
    SECTION ("sym") {
        
        env.define_variable(make_symbol("new-sym-var"), make_symbol("sym"));
        
        CHECK ( env.find(make_symbol("new-sym-var"))->is_sym() );
        CHECK ( env.find(make_symbol("new-sym-var"))->to_string().compare("sym") == 0 );
    }
    
    

}


TEST_CASE("Environment Test [define_function, define_macro]", "[env]")
{

    using namespace alisp;
    env::Environment env;
    eval::Evaluator eval(env);

    SECTION ("simple") {

        env.define_function(make_symbol("new-fun"),
                            make_object(make_symbol("a"), make_symbol("b")),
                            make_object(make_symbol("println"), make_symbol("a")));


        CHECK ( env.find(make_symbol("new-fun"))->check_function_flag() );
        CHECK ( env.find(make_symbol("new-fun"))->is_list() );
        CHECK ( env.find(make_symbol("new-fun"))->i(0)->is_list() );
        CHECK ( env.find(make_symbol("new-fun"))->i(1)->is_list() );
        
    }


    SECTION ("macro") {

        env.define_macro(make_symbol("new-fun"),
                         make_object(make_symbol("a"), make_symbol("b")),
                         make_object(make_symbol("println"), make_symbol("a")));


        CHECK ( env.find(make_symbol("new-fun"))->check_macro_flag() );
        CHECK ( env.find(make_symbol("new-fun"))->check_function_flag() );
        CHECK ( env.find(make_symbol("new-fun"))->is_list() );
        CHECK ( env.find(make_symbol("new-fun"))->i(0)->is_list() );
        CHECK ( env.find(make_symbol("new-fun"))->i(1)->is_list() );
        
    }

}


TEST_CASE("Environment Test [put]", "[env]")
{

    using namespace alisp;
    env::Environment env;
    eval::Evaluator eval(env);

    env.define_variable(make_symbol("new-int-var"), make_int(314));
    
    SECTION ("put on new scope") {
        env.new_scope();
        env.put(make_symbol("new-int-var"), make_int(42));
        
        CHECK ( env.find(make_symbol("new-int-var"))->is_int() );
        CHECK ( env.find(make_symbol("new-int-var"))->to_int() == 42 );

        env.destroy_scope();
    }

    SECTION ("put on new frame") {
        env.call_function();
        env.put(make_symbol("new-int-var"), make_int(42));
        
        CHECK ( env.find(make_symbol("new-int-var"))->is_int() );
        CHECK ( env.find(make_symbol("new-int-var"))->to_int() == 42 );

        env.finish_function();
    }

}


TEST_CASE("Environment Test [util]", "[env]")
{

    using namespace alisp;
    env::Environment env;
    eval::Evaluator eval(env);

    SECTION ("in funciton") {

        CHECK ( env.in_root() );
        
        CHECK ( !env.in_function() );
        
        env.call_function();
        CHECK ( env.in_function() );
        env.finish_function();

        CHECK ( !env.in_function() );
    }

    
    SECTION ("in root") {
        
        CHECK ( env.in_root() );
        
        env.call_function();
        CHECK ( !env.in_root() );
        env.finish_function();

        CHECK ( env.in_root() );

        env.new_scope();
        CHECK ( !env.in_root() );
        env.destroy_scope();

        CHECK ( env.in_root() );
    }


    SECTION ("call depth") {
        
        CHECK ( env.call_depth() == 0 );
        env.call_function();
        CHECK ( env.call_depth() == 1 );
        env.call_function();
        CHECK ( env.call_depth() == 2 );

        env.finish_function();
        CHECK ( env.call_depth() == 1 );
        env.finish_function();
        CHECK ( env.call_depth() == 0 );
        

    }
    
}


TEST_CASE("Environment Test [update]", "[env]")
{

    using namespace alisp;
    env::Environment env;
    eval::Evaluator eval(env);

    env.put(make_symbol("var"), make_int(42));
    CHECK ( env.find(make_symbol("var"))->to_int() == 42 );

    env.update(make_symbol("var"), make_int(314));
    CHECK ( env.find(make_symbol("var"))->to_int() == 314 );

    env.update(make_symbol("var"), make_double(314.42));
    CHECK ( env.find(make_symbol("var"))->to_real() == 314.42_a );
    
}


TEST_CASE("Environment Test [dumping]", "[env]")
{

    using namespace alisp;
    env::Environment env;
    eval::Evaluator eval(env);

    env.put(make_symbol("var"), make_int(42));
    env.update(make_symbol("var"), make_int(314));
    env.update(make_symbol("var"), make_double(314.42));
    env.put(make_symbol("var-2"), make_int(442));
    env.define_variable(make_symbol("var-3"), make_int(442));
    env.define_variable(make_symbol("var-4"), make_int(442));

    std::cout.setstate(std::ios_base::failbit);

    CHECK_NOTHROW( env.stack_dump() );
    CHECK_NOTHROW( env.callstack_dump() );

    env.trace_call("let");
    env.trace_call("when");
    env.trace_call("if");
    env.trace_call("while");
    
    CHECK_NOTHROW( env.stack_dump() );
    CHECK_NOTHROW( env.callstack_dump() );

    env.trace_unwind();
    env.trace_unwind();
    env.trace_unwind();
    env.trace_unwind();

    CHECK_NOTHROW( env.stack_dump() );
    CHECK_NOTHROW( env.callstack_dump() );
    
    std::cout.clear();
    
}
