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

#include "alisp/alisp/async/asyncs.hpp"

#include <chrono>


namespace alisp::async
{

uint32_t Future::new_future(al_callback t_calback)
{

    std::lock_guard<std::mutex> lock(Future::future_mutex);
    const auto id = future_registry.emplace_resource(Qnil, Qnil, Qnil, Qnil, Qnil, t_calback)->id;
    ++m_pending_futures;
    return id;
}

void Future::dispose_future(uint32_t t_id)
{
    std::lock_guard<std::mutex> lock(Future::future_mutex);

    if (!future_registry.belong(t_id))
    {
        return;
    }

    --m_pending_futures;
    future_registry.destroy_resource(t_id);
}

ALObjectPtr Future::future_resolved(uint32_t t_id)
{
    std::lock_guard<std::mutex> lock(Future::future_mutex);

    if (!future_registry.belong(t_id))
    {
        return Qnil;
    }

    return future_registry[t_id].resolved;
}

void Future::merge(uint32_t t_next, uint32_t t_current)
{
    future_registry[t_next].value            = future_registry[t_current].value;
    future_registry[t_next].success_state    = future_registry[t_current].resolved;
    future_registry[t_next].success_callback = future_registry[t_current].success_callback;
    future_registry[t_next].reject_callback  = future_registry[t_current].reject_callback;
    future_registry[t_next].internal         = future_registry[t_current].internal;
    future_registry[t_next].next_in_line     = future_registry[t_current].next_in_line;
    Future::dispose_future(t_next);
}

Future &Future::future(uint32_t t_id)
{
    return future_registry[t_id];
}


}  // namespace alisp::async
