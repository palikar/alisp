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

struct async_action
{

    static constexpr bool managed    = true;
    static constexpr bool has_future = true;
    
    ALObjectPtr g_action;
    ALObjectPtr g_callback;
    uint32_t g_future{0};
        
    async_action(ALObjectPtr action, ALObjectPtr callback) :
        g_action(std::move(action)), g_callback(std::move(callback))
    {}

    ALObjectPtr future(async::AsyncS *async)
    {
        if (pfunction(g_callback))
        {
            return Qt;
        }
        
        g_future = async->new_future();
        return resource_to_object(g_future);
    }

    ALObjectPtr operator()(async::AsyncS *async) const
    {

        async->submit_callback(g_action, nullptr, [callback = std::move(g_callback), async=async, future=g_future](auto value){
            
            if(future != 0)
            {
                        
                async->submit_future(future, value);
            }
            
            if (pfunction(callback))
            {
                async->submit_callback(callback, make_list(value));
            }
            
        });        
        
        return Qt;
    }
};


}  // namespace alisp
