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

#include "alisp/config.hpp"

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/declarations/constants.hpp"

#include "alisp/alisp/async/event.hpp"

#include "alisp/management/registry.hpp"

#include <iostream>
#include <vector>
#include <thread>
#include <string>
#include <functional>
#include <string>
#include <mutex>
#include <atomic>
#include <condition_variable>
#include <memory>
#include <utility>
#include <queue>


namespace alisp
{

namespace async
{

struct Future
{
    ALObjectPtr value;
    ALObjectPtr resolved;

    ALObjectPtr success_state;

    // callbacks
    ALObjectPtr success_callback;
    ALObjectPtr reject_callback;

    // c++ space things
    std::function<void(ALObjectPtr)> internal{};

    uint32_t next_in_line{0};

    static std::mutex future_mutex;

    static inline std::atomic_uint_fast32_t m_pending_futures{ 0 };

    static uint32_t new_future(al_callback t_calback = {});

    static void dispose_future(uint32_t t_id);

    static Future &future(uint32_t t_id);

    static ALObjectPtr future_resolved(uint32_t t_id);

    static void merge(uint32_t t_next, uint32_t t_current);
    
};


inline management::Registry<Future, 0x05> future_registry{};




}  // namespace async

}  // namespace alisp
