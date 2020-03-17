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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_parser.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/modules/modules_inits.hpp"

#include <string>
#include <vector>
#include <iostream>

using Catch::Matchers::Equals;
using namespace Catch::literals;


TEST_CASE("Base64 Test", "[base64]")
{
    using namespace alisp;

    env::Environment env;
    auto p = std::make_shared<parser::ALParser<alisp::env::Environment>>(env);
    eval::Evaluator eval(env, p.get());

    // auto base64 = init_base64(&env, &eval);
}
