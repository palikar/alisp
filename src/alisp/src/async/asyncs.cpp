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

AsyncS::AsyncS(eval::Evaluator *t_eval, bool defer_init) : m_eval(t_eval), m_flags(0), m_thread_pool{ 2 }
{
    AL_BIT_OFF(m_flags, INIT_FLAG);

    if (!defer_init)
    {
        init();
    }
}

void AsyncS::init()
{

    AL_BIT_ON(m_flags, RUNNING_FLAG);
    m_event_loop = std::thread(&AsyncS::event_loop, this);
    while (!AL_BIT_CHECK(m_flags, INIT_FLAG))
    {
    }
}

void AsyncS::check_exit_condition()
{

    if (!m_callback_queue.empty())
    {
        return;
    }

    if (!m_event_queue.empty())
    {
        return;
    }

    if (m_asyncs != 0)
    {
        return;
    }

    if (m_eval->is_interactive())
    {
        return;
    }

    if (AL_BIT_CHECK(m_flags, AWAIT_FLAG))
    {
        return;
    }

    if (AL_BIT_CHECK(m_flags, UR_FLAG))
    {
        return;
    }

    {
        std::lock_guard<std::mutex> guard{ Future::future_mutex };
        if (Future::m_pending_futures != 0)
        {
            return;
        }
    }

    {
        std::lock_guard<std::mutex> guard{ timers_mutex };
        if (m_pending_timers != 0)
        {
            return;
        }
    }

    AL_BIT_OFF(m_flags, RUNNING_FLAG);
    m_eval->reset_async_flag();
    m_eval->callback_cv.notify_all();
}

void AsyncS::handle_timers()
{
    std::lock_guard<std::mutex> guard{ timers_mutex };
    auto it = m_timers.begin();
    while (it != m_timers.end())
    {
        if (it->time < m_now)
        {

            submit_callback(it->callback, nullptr, it->internal_callback);

            if (is_falsy(it->periodic))
            {
                it = m_timers.erase(it);
                --m_pending_timers;
                continue;
            }
        }

        ++it;
    }
    // spin_loop();
}

void AsyncS::handle_actions()
{

    auto it = m_actions_queue.begin();
    while (it != m_actions_queue.end())
    {

        if (it->valid and !it->executing)
        {
            m_thread_pool.submit([&, action = it]() {
                action->executing = true;
                action->operator()(this);
                action->executing = false;
            });
        }

        ++it;
    }
}

void AsyncS::event_loop()
{

    using namespace std::chrono_literals;
    std::unique_lock<std::mutex> el_lock{ event_loop_mutex, std::defer_lock };
    AL_BIT_ON(m_flags, INIT_FLAG);
    AL_BIT_ON(m_flags, RUNNING_FLAG);
    while (AL_BIT_CHECK(m_flags, RUNNING_FLAG))
    {
        event_loop_cv.wait_for(el_lock, 10ms);

        m_now = Timer::now();

        handle_timers();
        // m_thread_pool.submit([&]() { handle_timers(); });

        if (!AL_BIT_CHECK(m_flags, RUNNING_FLAG))
        {
            return;
        }

        while (!m_event_queue.empty())
        {
            execute_event(std::move(m_event_queue.front()));
            ++m_asyncs;
            m_event_queue.pop();
        }

        handle_actions();

        if (!m_callback_queue.empty())
        {
            if (AL_BIT_CHECK(m_flags, AWAIT_FLAG))
            {
                execute_callback(std::move(m_callback_queue.front()));
                m_callback_queue.pop();
                continue;
            }
            m_eval->callback_cv.notify_all();
            continue;
        }

        check_exit_condition();
    }
}

void AsyncS::execute_event(event_type call)
{

    std::thread tr([&, call = std::move(call)] {
        call(this);
        --m_asyncs;
        m_eval->callback_cv.notify_all();
        spin_loop();
    });

    tr.detach();
}

void AsyncS::execute_callback(callback_type call)
{
    auto &function = call.function;
    auto &args     = call.arguments;
    auto &internal = call.internal;

    auto res = [&] {
        eval::detail::EvaluationLock lock{ *m_eval };
        return m_eval->eval_callable(function, args == nullptr ? make_list() : args);
    }();

    if (internal)
    {
        internal(res);
    }

    spin_loop();
}

void AsyncS::spin_loop()
{
#ifndef MULTI_THREAD_EVENT_LOOP
    event_loop_cv.notify_one();
#else
    pool_cv.notify_all();
#endif
}

void AsyncS::submit_event(event_type t_callback)
{

    if (!AL_BIT_CHECK(m_flags, INIT_FLAG))
    {
        init();
    }

    {

#ifndef MULTI_THREAD_EVENT_LOOP
        std::lock_guard<std::mutex> guard{ event_loop_mutex };
#else
        std::lock_guard<std::mutex> guard{ event_queue_mutex };
#endif


        m_event_queue.push(std::move(t_callback));

        m_eval->set_async_flag();
    }

    spin_loop();
}

void AsyncS::submit_callback(ALObjectPtr function, ALObjectPtr args, std::function<void(ALObjectPtr)> internal)
{

    if (!AL_BIT_CHECK(m_flags, INIT_FLAG))
    {
        init();
    }

    if (m_eval->is_interactive() or AL_BIT_CHECK(m_flags, AWAIT_FLAG))
    {
        execute_callback({ function, args, internal });
    }
    else
    {
        {
            std::lock_guard<std::mutex> guard(callback_queue_mutex);
            if (args == nullptr)
            {
                m_callback_queue.push({ function, make_list(), internal });
            }
            else
            {
                m_callback_queue.push({ function, args });
            }
        }

        m_eval->callback_cv.notify_all();
        spin_loop();
    }
}

void AsyncS::submit_future(uint32_t t_id, ALObjectPtr t_value, bool t_good)
{
    if (!AL_BIT_CHECK(m_flags, INIT_FLAG))
    {
        init();
    }

    std::lock_guard<std::mutex> lock(Future::future_mutex);

    if (!future_registry.belong(t_id))
    {
        return;
    }

    auto &fut = future_registry[t_id];

    fut.value         = t_value;
    fut.success_state = t_good ? Qt : Qnil;
    fut.resolved      = Qt;

    m_eval->futures_cv.notify_all();

    if (t_good and pfunction(fut.success_callback))
    {
        submit_callback(fut.success_callback, make_list(t_value), [&, next = fut.next_in_line](auto res) {
            if (next > 0)
            {

                if (pint(res) and future_registry.belong(object_to_resource(res)))
                {
                    auto other = object_to_resource(res);
                    Future::merge(other, next);
                    return;
                }

                submit_future(next, res);
                spin_loop();
            }
        });
    }
    else if (!t_good and pfunction(fut.reject_callback))
    {
        submit_callback(fut.reject_callback, make_list(t_value));
    }

    if (fut.internal)
    {
        fut.internal(fut.value);
    }
}

void AsyncS::submit_timer(Timer::time_point time, ALObjectPtr function, ALObjectPtr periodic, al_callback internal)
{
    if (!AL_BIT_CHECK(m_flags, INIT_FLAG))
    {
        init();
    }
    m_eval->set_async_flag();
    std::lock_guard<std::mutex> guard{ timers_mutex };
    m_timers.push_back({ time + m_now.time_since_epoch(), function, internal, periodic });
    ++m_pending_timers;
    spin_loop();
}

void AsyncS::async_pending()
{
    AL_BIT_ON(m_flags, UR_FLAG);
    m_eval->set_async_flag();
}

void AsyncS::async_reset_pending()
{
    AL_BIT_OFF(m_flags, UR_FLAG);
    m_eval->reset_async_flag();
}

bool AsyncS::has_callback()
{
    std::lock_guard<std::mutex> guard(callback_queue_mutex);
    return !m_callback_queue.empty();
}

AsyncS::callback_type AsyncS::next_callback()
{
    std::lock_guard<std::mutex> guard(callback_queue_mutex);
    auto callback = std::move(m_callback_queue.front());
    m_callback_queue.pop();
    event_loop_cv.notify_all();
    return callback;
}

void AsyncS::dispose()
{
    AL_BIT_OFF(m_flags, RUNNING_FLAG);
    spin_loop();

#ifndef MULTI_THREAD_EVENT_LOOP
    if (m_event_loop.joinable())
    {
        m_event_loop.join();
    }

#else
    for (size_t i = 0; i < 3; ++i)
    {
        if (pool[i].joinable())
        {
            pool[i].join();
        }
    }

#endif
}


}  // namespace alisp::async
