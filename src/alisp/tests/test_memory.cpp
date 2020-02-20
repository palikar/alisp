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


TEST_CASE("Memory Test [simple]", "[memory]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    auto input = R"((import 'memory) (defvar mem (memory.buffer-allocate 32))
(memory.buffer-release mem))"s;
    
    CHECK( engine.eval_statement(input).first );
    
    std::cout.clear();
}



TEST_CASE("Memory Test [reading and writing 1]", "[memory]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    auto input = R"((import 'memory) (defvar mem (memory.buffer-allocate 32))

(memory.buffer-nth-set mem 1 128)
(assert (== (memory.buffer-nth-get mem 1 ) 128))
(memory.buffer-release mem))"s;
    
    CHECK( engine.eval_statement(input).first );
    
    std::cout.clear();
}


TEST_CASE("Memory Test [reading and writing 2]", "[memory]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    auto input = R"((import 'memory) (defvar mem (memory.buffer-allocate 32))

(memory.buffer-fill mem 128)
(assert (== (memory.buffer-nth-get mem 1 ) 128))
(memory.buffer-release mem))"s;
    
    CHECK( engine.eval_statement(input).first );
    
    std::cout.clear();
}

TEST_CASE("Memory Test [reading and writing 3]", "[memory]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);

    auto input = R"((import 'memory) 
(defvar mem-1 (memory.buffer-allocate 32))
(defvar mem-2 (memory.buffer-allocate 32))

(memory.buffer-fill mem-1 128)
(memory.buffer-fill mem-2 64)
(memory.buffer-set mem-1 (memory.buffer-get mem-2))
(assert (equal (memory.buffer-get mem-1) (memory.buffer-get mem-2 )))
(memory.buffer-release mem-1)
(memory.buffer-release mem-2)
)"s;
    
    CHECK( engine.eval_statement(input).first );
    
    std::cout.clear();
}
