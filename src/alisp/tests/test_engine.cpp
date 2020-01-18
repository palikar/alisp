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



TEST_CASE("Engine Test [simple]", "[engine]")
{
    using namespace alisp;

    LanguageEngine engine;

    std::cout.setstate(std::ios_base::failbit);
    
    SECTION ("basic") {
        std::string input{"(defvar var 42) (setq var (+ var 1))"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }

    std::cout.clear();
}


TEST_CASE("Engine Test [imports]", "[engine]")
{
    using namespace alisp;

    LanguageEngine engine;
    std::cout.setstate(std::ios_base::failbit);
    
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
    
    std::cout.clear();    
}


TEST_CASE("Engine Test [settings]", "[engine]")
{
    using namespace alisp;

    std::cout.setstate(std::ios_base::failbit);
    
    LanguageEngine engine({EngineSettings::PARSER_DEBUG, EngineSettings::EVAL_DEBUG});
    SECTION ("basic") {
        std::string input{"(println \"Hello world\")"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }

    std::cout.clear();
}


TEST_CASE("Engine Test [args]", "[engine]")
{
    using namespace alisp;

    std::cout.setstate(std::ios_base::failbit);
    
    LanguageEngine engine({}, {"-one -two"});
    
    SECTION ("basic") {
        std::string input{"(dump --argv-- )"};
        CHECK_NOTHROW( engine.eval_statement(input) );
    }
    
    std::cout.clear();
}
