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

#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/utility/macros.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/declarations/constants.hpp"

#include <iostream>
#include <vector>
#include <thread>
#include <string>
#include <functional>
#include <string>
#include <mutex>
#include <atomic>
#include <memory>
#include <utility>
#include <queue>
#include <chrono>

namespace alisp::async
{


struct FSNotify
{
    ALObjectPtr callback;


    bool init() {}


    ALObjectPtr operator()(AsyncS *async) { return false; }
};


}  // namespace alisp::async
