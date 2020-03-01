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


TEST_CASE("Evaluating Files Test [simple]", "[files]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    CHECK_NOTHROW(engine.eval_file(EXAMPLE_FILE));

    std::cout.clear();
}

TEST_CASE("Reading Files Test [simple]", "[files]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);


    auto input = R"((defvar file-1  (file-open ")"s += std::string(TEXT_FILE) +=
      R"(" :in) )
(assert (string-equals "line 1" (file-read-line file-1)))
(assert (string-equals "line 2" (file-read-line file-1)))
(assert (string-equals "line 3" (file-read-line file-1)))
(file-close file-1)
)"s;

    CHECK(engine.eval_statement(input, true).first);

    std::cout.clear();
}

TEST_CASE("Writing Files Test [simple]", "[files]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    auto input = R"((defvar file-1  (file-open ")"s += std::string(OUTPUT_FILE) += R"(" :out) )
(file-write-line file-1 "line 1")
(file-write-line file-1 "line 2")
(file-write-line file-1 "line 3")
(file-close file-1)
)"s;

    CHECK(engine.eval_statement(input, true).first);

    input = R"((defvar file-in  (file-open ")"s += std::string(OUTPUT_FILE) +=
      R"(" :in) )
(assert (string-equals "line 1" (file-read-line file-in)))
(assert (string-equals "line 2" (file-read-line file-in)))
(assert (string-equals "line 3" (file-read-line file-in)))
(file-close file-in)
)"s;


    input = R"((defvar file-in  (file-open ")"s += std::string(OUTPUT_FILE) +=
      R"(" :in) )
(while (file-has-more file-in)
(println (file-read-line file-in)))
(file-close file-in)
)"s;

    CHECK(engine.eval_statement(input, true).first);

    std::cout.clear();
}
