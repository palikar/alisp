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


#include "alisp/alisp/async/future.hpp"
#include "alisp/alisp/async/event.hpp"
#include "alisp/alisp/async/thread_pool.hpp"

#include "alisp/management/registry.hpp"

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


namespace alisp
{

namespace eval
{
class Evaluator;
}

namespace async
{


class AsyncS
{
  public:
    static constexpr size_t POOL_SIZE = 3;
    using callback_type               = detail::CallbackObject;
    using event_type                  = detail::EventObject;

    static constexpr std::uint32_t RUNNING_FLAG     = 0x0001;
    static constexpr std::uint32_t EL_SPINNING_FLAG = 0x0002;
    static constexpr std::uint32_t INIT_FLAG        = 0x0004;
    static constexpr std::uint32_t AWAIT_FLAG       = 0x0008;
    static constexpr std::uint32_t UR_FLAG          = 0x0010;

  private:
    eval::Evaluator *m_eval;

    std::queue<event_type> m_event_queue;
    std::queue<callback_type> m_callback_queue;
    std::thread m_event_loop;
    std::atomic_uint32_t m_flags;

    std::atomic_int m_asyncs{ 0 };

    mutable std::mutex callback_queue_mutex;

#ifndef MULTI_THREAD_EVENT_LOOP

    mutable std::mutex event_loop_mutex;
    mutable std::condition_variable event_loop_cv;

    void event_loop();
#else

    std::thread pool[POOL_SIZE];

    mutable std::mutex pool_mutex;
    mutable std::condition_variable pool_cv;
    mutable std::mutex event_queue_mutex;

    void event_loop_thread();
#endif

    thread_pool::ThreadPool m_thread_pool;

    void execute_event(event_type call);

    void execute_callback(callback_type call);

    void init();

  public:
    AsyncS(eval::Evaluator *t_eval, bool defer_init = false);

    void spin_loop();


    void submit_event(event_type t_event);

    void submit_callback(ALObjectPtr function, ALObjectPtr args = nullptr, al_callback internal = {});

    void submit_future(uint32_t t_id, ALObjectPtr t_value, bool t_good = true);
    
    
    void async_pending();

    void async_reset_pending();

    bool has_callback();

    callback_type next_callback();

    inline std::uint32_t status_flags() { return m_flags; }

    inline void start_await() { AL_BIT_ON(m_flags, AWAIT_FLAG); }

    inline void end_await() { AL_BIT_OFF(m_flags, AWAIT_FLAG); }

    void dispose();
};


class Await
{
  public:
    explicit Await(AsyncS &t_async);
    ~Await();

    ALISP_RAII_OBJECT(Await);

  private:
    AsyncS &m_async;
};


}  // namespace async

}  // namespace alisp
