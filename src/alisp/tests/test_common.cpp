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
        CHECK( make_object(std::string{"string1"}, std::string{"string2"})->is_list() );
        CHECK( make_object(42)->is_int() );
        CHECK( make_object(42.32)->is_real() );
        CHECK( make_object(1,2,3,4,5)->is_list() );

        CHECK( make_symbol("sym")->is_sym() );
        CHECK( make_int(12)->is_int() );
        CHECK( make_double(12.12)->is_real() );
        CHECK( make_list(make_int(12))->is_list() );

    }


}


TEST_CASE("Common Test [util]", "[common]")
{
    using namespace alisp;


    SECTION ( "splice[1]" ) {
        auto obj = splice(make_object(1,2,3,4,5), 1);

        CHECK( obj->is_list() );
        CHECK( obj->length() ==  4);
        CHECK( obj->i(0)->is_int() );
        CHECK( obj->i(0)->to_int() == 2 );
    }

    SECTION ( "splice[2]" ) {
        auto obj = splice(make_object(1,2,3,4,5), 1, 4);

        CHECK( obj->is_list() );
        CHECK( obj->length() ==  3);
        CHECK( obj->i(0)->is_int() );
        CHECK( obj->i(0)->to_int() == 2 );
    }

    SECTION ( "falsey" ) {

        CHECK( is_falsy(make_object("")));
        CHECK( is_falsy(make_object(std::vector<ALObject*>{})));
        CHECK( is_falsy(make_object(0)));

    }

    SECTION ( "truthy" ) {
        CHECK( is_truthy(make_object("string")));
        CHECK( is_truthy(make_object(1,2,3)));
        CHECK( is_truthy(make_object(12)));
    }

    SECTION ( "predicates" ) {

        CHECK( are_objects_int(make_object(1,2,3)) );
        CHECK( are_objects_real(make_object(1.3, 2.4, 3.5)) );
        CHECK( !are_objects_real(make_object(1.3, "asd", "asdf")) );

        CHECK( min_list_elements(make_object("sad", "asd", "asdf"), 2) );
        CHECK( max_list_elements(make_object("sad", "asd", "asdf"), 5) );

        CHECK( !min_list_elements(make_object("sad", "asd", "asdf"), 4) );
        CHECK( !max_list_elements(make_object("sad", "asd", "asdf"), 2) );

        CHECK( pstring(make_object("string")) );
        CHECK( plist(make_object("string1", "string2")) );
        CHECK( pint(make_object(42)) );
        CHECK( preal(make_object(42.32)) );
        CHECK( plist(make_object(1,3,4,5)));

    }

}


TEST_CASE("Common Test [pattern matching]", "[common]")
{
    using namespace alisp;

    SECTION ("strings") {
        CHECK_THROWS ( make_visit(make_object("string"),
                                  type(ALObjectType::INT_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::REAL_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::STRING_VALUE ) >>=  [](ALObject*) { throw "exc"; },
                                  type(ALObjectType::SYMBOL ) >>=  [](ALObject*) {}
                           ) );
    }

    SECTION ("int") {
        CHECK_THROWS ( make_visit(make_object(1),
                                  type(ALObjectType::INT_VALUE ) >>=  [](ALObject*) { throw "exc"; },
                                  type(ALObjectType::REAL_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::STRING_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::SYMBOL ) >  [](ALObject*) {}
                           ) );
    }

    SECTION ("strings") {
        CHECK_THROWS ( make_visit(make_object(1.3),
                                  type(ALObjectType::INT_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::REAL_VALUE ) >>=  [](ALObject*)  { throw "exc"; },
                                  type(ALObjectType::STRING_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::SYMBOL ) >>=  [](ALObject*) {}
                           ) );
    }

    SECTION ("sym") {
        CHECK_THROWS ( make_visit(make_symbol("sym"),
                                  type(ALObjectType::INT_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::REAL_VALUE ) >>=  [](ALObject*)  {},
                                  type(ALObjectType::STRING_VALUE ) >>=  [](ALObject*) {},
                                  type(ALObjectType::SYMBOL ) >  [](ALObject*) { throw "exc"; }
                           ) );
    }

    SECTION ("list") {
        CHECK_THROWS ( make_visit(make_object(1, 2, 3),
                                  type(ALObjectType::INT_VALUE ) >  [](ALObject*) {},
                                  type(ALObjectType::REAL_VALUE ) >  [](ALObject*)  {},
                                  type(ALObjectType::STRING_VALUE ) >  [](ALObject*) {},
                                  type(ALObjectType::LIST ) >  [](ALObject*) { throw "exc"; },
                                  type(ALObjectType::SYMBOL ) >  [](ALObject*) {}
                           ) );
    }

}


TEST_CASE("Common Test [equal]", "[equal]")
{
    using namespace alisp;


    SECTION ( "simple equal" ) {

        CHECK( equal(make_object("string"), make_object("string")) );
        CHECK( !equal(make_object("string"), make_object("string-1")) );

        CHECK( equal(make_object(1), make_object(1)) );
        CHECK( equal(make_object(12), make_object(12)) );

        CHECK( equal(make_object(1.1), make_object(1.1)) );
        CHECK( !equal(make_object(12.1), make_object(12.5)) );


        CHECK( equal(make_symbol("foo"), make_symbol("foo")) );
        CHECK( !equal(make_symbol("foo"), make_symbol("bar")) );
    }

    SECTION ( "list equal" ) {

        CHECK( !equal(make_object("string", 12), make_object("string")) );
        CHECK( equal(make_object("string", 12), make_object("string", 12)) );

        CHECK( equal(make_object("string", 12, 2.3), make_object("string", 12, 2.3)) );

        CHECK( !equal(make_object("string", 12, make_object(1,2,3)), make_object("string", 12, 2.3)) );
        CHECK( equal(make_object("string", 12, make_object(1,2,3)), make_object("string", 12, make_object(1,2,3))) );

    }


    SECTION ( "simple eq" ) {

        CHECK( eq(make_object("string"), make_object("string")) );
        CHECK( !eq(make_object("string"), make_object("string-1")) );

        CHECK( eq(make_object(1), make_object(1)) );
        CHECK( eq(make_object(12), make_object(12)) );

        CHECK( eq(make_object(1.1), make_object(1.1)) );
        CHECK( !eq(make_object(12.1), make_object(12.5)) );


        auto foo = make_symbol("foo");
        CHECK( !eq(make_symbol("foo"), make_symbol("foo")) );
        CHECK( eq(foo, foo) );

        CHECK( !eq(make_object("string", 12, make_object(1,2,3)), make_object("string", 12, 2.3)) );
        CHECK( !eq(make_object("string", 12, make_object(1,2,3)), make_object("string", 12, make_object(1,2,3))) );

        auto l = make_object("string", 12, make_object(1,2,3));
        CHECK( eq(l, l) );
        
        
    }


}
