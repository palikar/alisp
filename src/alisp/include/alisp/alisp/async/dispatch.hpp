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
#pragma once

#include "alisp/alisp/async/asyncs.hpp"

namespace alisp
{

namespace eval
{
class Evaluator;
}

namespace async
{

template<typename T, typename... Args> auto dispatch(AsyncS &async, Args &&... args)
{

    if constexpr (T::managed)
    {
        T event_object{ std::forward<decltype(args)>(args)... };


        if constexpr (T::has_future)
        {
            auto fut = event_object.future(&async);
            async.submit_event(detail::EventObject{ std::move(event_object) });
            return fut;
        }
        else
        {
            async.submit_event(detail::EventObject{ std::move(event_object) });
            return Qt;
        }
    }
    else
    {
        T event_object{ std::forward<decltype(args)>(args)... };
        return event_object(&async);
    }
}

}  // namespace async

}  // namespace alisp
