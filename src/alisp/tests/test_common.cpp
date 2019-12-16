#include "catch2/catch.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include <string>
#include <vector>

using Catch::Matchers::Equals;
using namespace Catch::literals;



TEST_CASE("Common Test [make]", "[common]")
{
    using namespace alisp;


    SECTION ( "simple-creating" ) {

        CHECK( make_object("string")->is_string() );
        CHECK( make_object("string1", "string2")->is_list() );
        CHECK( make_object(42)->is_int() );
        CHECK( make_object(42.32)->is_real() );
        CHECK( make_object(std::vector<int>{1,3,4,5})->is_list() );
        
        CHECK( make_symbol("sym")->is_sym() );

        CHECK( make_int(12)->is_int() );

        CHECK( make_double(12.12)->is_real() );

        CHECK( make_list(make_int(12.3))->is_list() );

    }


}



TEST_CASE("Common Test [util]", "[common]")
{
    using namespace alisp;


    SECTION ( "splice[1]" ) {
        auto obj = splice(make_object(std::vector<int>{1,2,3,4,5}), 1);
        CHECK( obj->is_list() );
        CHECK( obj->length() ==  4);
        CHECK( obj->i(0)->is_int() );
        CHECK( obj->i(0)->to_int() == 2 );
        
    }

    SECTION ( "splice[2]" ) {
        auto obj = splice(make_object(std::vector<int>{1,2,3,4,5}), 1, 4);
        CHECK( obj->is_list() );
        CHECK( obj->length() ==  3);
        CHECK( obj->i(0)->is_int() );
        CHECK( obj->i(0)->to_int() == 4 );
        
    }

    SECTION ( "falsey" ) {
        
        CHECK( is_falsy(make_obj("")));
        CHECK( is_falsy(make_obj(std::vector<int>{})));
        CHECK( is_falsy(make_obj(0)));

    }

    SECTION ( "falsey" ) {
        CHECK( is_truthy(make_obj("string")));
        CHECK( is_truthy(make_obj(std::vector<int>{1,2,3})));
        CHECK( is_truthy(make_obj(12)));
    }

    SECTION ( "predicates" ) {

        CHECK( are_objects_int(make_obj(std::vector<int>{1,2,3})) );
        CHECK( are_objects_real(make_obj(std::vector<double>{1.3, 2.4, 3.5})) );

        CHECK( are_objects_real(make_obj("sad", "asd", "asdf")) );

        CHECK( min_list_elements(make_obj("sad", "asd", "asdf"), 2) );
        CHECK( max_list_elements(make_obj("sad", "asd", "asdf"), 5) );

        CHECK( !min_list_elements(make_obj("sad", "asd", "asdf"), 4) );
        CHECK( !max_list_elements(make_obj("sad", "asd", "asdf"), 2) );

        CHECK( pstring(make_object("string")) );
        CHECK( plist(make_object("string1", "string2")) );
        CHECK( pint(make_object(42)) );
        CHECK( preal(make_object(42.32)) );
        CHECK( plist(make_object(std::vector<int>{1,3,4,5})) );
        
    }
    
}
