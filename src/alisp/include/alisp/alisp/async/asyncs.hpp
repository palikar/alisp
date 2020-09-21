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
#include <chrono>


namespace alisp
{

namespace eval
{
class Evaluator;
}

namespace async
{

struct Timer
{
    using time_point = std::chrono::time_point<std::chrono::high_resolution_clock>;

    using time_duration = std::chrono::milliseconds;

    static time_point now() { return std::chrono::high_resolution_clock::now(); }


    time_point time;
    time_duration duration;
    ALObjectPtr callback;
    al_callback internal_callback{};
    ALObjectPtr periodic{ Qnil };
};

class AsyncS
{
  public:
    static constexpr size_t POOL_SIZE = 3;
    using callback_type               = detail::CallbackObject;
    using work_type                   = detail::WorkObject;
    using action_type                 = detail::ActionObject;

    static constexpr std::uint32_t RUNNING_FLAG     = 0x0001;
    static constexpr std::uint32_t EL_SPINNING_FLAG = 0x0002;
    static constexpr std::uint32_t INIT_FLAG        = 0x0004;
    static constexpr std::uint32_t AWAIT_FLAG       = 0x0008;
    static constexpr std::uint32_t UR_FLAG          = 0x0010;

  private:
    eval::Evaluator *m_eval;

    std::queue<work_type> m_work_queue;

    std::queue<callback_type> m_callback_queue;
    mutable std::mutex callback_queue_mutex;

    std::vector<action_type> m_actions_queue;

    mutable std::mutex action_queue_mutex;

    std::atomic_uint32_t m_flags;
    std::atomic_int m_asyncs{ 0 };
    Timer::time_point m_now;
    std::vector<Timer> m_timers;
    mutable std::mutex timers_mutex;
    mutable size_t m_pending_timers{ 0 };

    thread_pool::ThreadPool m_thread_pool;

    std::thread m_event_loop;
    mutable std::mutex event_loop_mutex;
    mutable std::condition_variable event_loop_cv;
    void event_loop();

    void execute_work(work_type call);

    void execute_callback(callback_type call);

    void init();

    void check_exit_condition();

    void handle_timers();

    void handle_actions();

    void handle_work();

  public:
    AsyncS(eval::Evaluator *t_eval, bool defer_init = false);

    void spin_loop();


    void submit_work(work_type t_event);

    void submit_action(action_type t_event);

    void submit_callback(ALObjectPtr function, ALObjectPtr args = nullptr, al_callback internal = {});

    void submit_future(uint32_t t_id, ALObjectPtr t_value, bool t_good = true);

    void submit_timer(Timer::time_duration time, ALObjectPtr function, ALObjectPtr periodic, al_callback internal = {});


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
