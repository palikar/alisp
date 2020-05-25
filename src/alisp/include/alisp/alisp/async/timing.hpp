/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

     This program is free software; you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     n the Free Software Foundation; either version 2 of the License, or
     (at your option) any prior version.

     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
     GNU General Public License for more details.

     You should have received a copy of the GNU General Public License along
     with this program; if not, write to the Free Software Foundation, Inc.,
     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#pragma once

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_asyncs.hpp"
#include "alisp/alisp/declarations/constants.hpp"
#include "alisp/alisp/alisp_object.hpp"

namespace alisp
{


struct set_timeout
{
    static constexpr bool managed    = true;
    static constexpr bool has_future = false;
    size_t milliseconds;
    ALObjectPtr callback;

    set_timeout(size_t t_miliseconds, ALObjectPtr t_callback)
      : milliseconds(t_miliseconds), callback(std::move(t_callback))

    {
    }

    ALObjectPtr operator()(async::AsyncS *async) const
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(milliseconds));
        async->submit_callback(callback);
        return Qt;
    }
};

struct future_int
{
    static constexpr bool managed    = true;
    static constexpr bool has_future = true;

    int g_value;
    uint32_t g_future;

    future_int(int t_value) : g_value(t_value), g_future(0) {}

    ALObjectPtr future(async::AsyncS *async)
    {
        g_future = async->new_future();
        return resource_to_object(g_future);
    }

    ALObjectPtr operator()(async::AsyncS *async) const
    {
        std::this_thread::sleep_for(std::chrono::milliseconds(1500));
        async->submit_future(g_future, make_int(g_value));
        return Qt;
    }
};


}  // namespace alisp
