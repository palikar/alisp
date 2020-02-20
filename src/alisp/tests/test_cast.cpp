/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation; either version 2 of the License, or
     (at your option) any prior version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License along
     with this program; if not, write to the Free Software Foundation, Inc.,
     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */


#include "catch2/catch.hpp"

#include "alisp/alisp/alisp_engine.hpp"

#include <string>
#include <vector>
#include <iostream>

using Catch::Matchers::Equals;
using namespace Catch::literals;


TEST_CASE("Casting Test [int]", "[cast]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input{ "(assert (== 12 (parse-int \"12\") ))" };
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}

TEST_CASE("Casting Test [float]", "[cast]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input{ "(assert (== 12.2 (parse-float \"12.2\") ))" };
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}

TEST_CASE("Casting Test [string]", "[cast]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input = R"b(
 (assert (string-equals "12.2" (to-string 12.2)))
 (assert (string-equals "12"   (to-string 12)))
 (assert (string-equals "12"   (to-string "12")))
)b";

    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}


TEST_CASE("Casting Test [char]", "[cast]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input = R"b(
 (assert (== ?a (to-char "a")))
 (assert (== ?a (to-char 97)))
 
)b";

    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}
