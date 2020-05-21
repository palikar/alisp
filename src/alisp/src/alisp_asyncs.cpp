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

#include "alisp/alisp/alisp_asyncs.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include <chrono>



namespace alisp
{

namespace async
{


AsyncS::AsyncS(eval::Evaluator *t_eval) : m_eval(t_eval)
{
    m_event_loop = std::thread(&AsyncS::event_loop, this);
    running = 1;
    // m_event_loop.detach();
}

void AsyncS::event_loop()
{
    using namespace std::chrono_literals;
    std::unique_lock<std::mutex> el_lock{event_loop_mutex};
    while (running)
    {
        event_loop_cv.wait_for(el_lock, 100ms);

        if (!m_event_queue.empty())
        {
            ++asyncs;
            execute(std::move(m_event_queue.back()));
            m_event_queue.pop();
        }

        if (m_callback_queue.empty() and m_event_queue.empty() and asyncs == 0)
        {
            running = 0;
            break;
        }

    }

    m_eval->reset_async_flag();
    m_eval->callback_cv.notify_all();
}

void AsyncS::execute(detail::Callback call)
{
    call();
    std::lock_guard<std::mutex> guard(queue_mutex);
    m_callback_queue.push(432);
    --asyncs;
    m_eval->callback_cv.notify_all();
}

void AsyncS::eval_callback(detail::Callback)
{

}

void AsyncS::submit(detail::Callback t_callback)
{
    std::lock_guard<std::mutex> guard{event_loop_mutex};
    m_event_queue.push(std::move(t_callback));
    m_eval->set_async_flag();
    event_loop_cv.notify_one();
}


bool AsyncS::has_callback()
{
    std::lock_guard<std::mutex> guard(queue_mutex);
    const auto value = !m_callback_queue.empty();
    return value;
}

int AsyncS::next_callback()
{
    std::lock_guard<std::mutex> guard(queue_mutex);
    auto value = m_callback_queue.back();
    m_callback_queue.pop();
    event_loop_cv.notify_all();
    return value;
}

void AsyncS::end()
{
    if (m_event_loop.joinable())
    {
        m_event_loop.join();
    }

}

}  // namespace async

}  // namespace alisp
