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


TEST_CASE("Algorithms Test [all]", "[alg]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input{ "(assert (all (lambda (x) (> x 0)) '(1 2 3 4 5 6)))" };
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}


TEST_CASE("Algorithms Test [zip]", "[alg]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input{
        "(all (lambda (x) (assert (and (< 0 (nth x 0)) (> 0 (nth x 1)))) ) "
        "(zip '(1 2 3 4) '(-1 -2 -3 -4)) )"
    };
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}


TEST_CASE("Algorithms Test [filter]", "[alg]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input{
        "(all (lambda (x) (assert (< 0 x) )) (filter (lambda (x)  (< 0 x)) "
        "'(-1 2 -3 4 -5 10)))"
    };
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}

TEST_CASE("Algorithms Test [sort]", "[alg]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input = R"b( (defvar s (sort '( 10 2 -1 4)))
 (assert (== -1 (nth s 0)))
 (assert (==  2 (nth s 1)))
 (assert (==  4 (nth s 2)))
 (assert (==  10 (nth s 3))) )b";
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}


TEST_CASE("Algorithms Test [any]", "[alg]")
{
    using namespace alisp;
    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    std::string input{ "(assert (any (lambda (x) (> x 0)) '(-1 -2 -3 -4 -5 6)))" };
    CHECK(engine.eval_statement(input).first);

    std::cout.clear();
}
