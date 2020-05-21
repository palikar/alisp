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


namespace alisp
{

namespace async
{

AsyncS::AsyncS(eval::Evaluator *t_eval) : m_eval(t_eval)
{
    m_event_loop = std::thread(&AsyncS::event_loop, this);
    // m_event_loop.detach();
}

void AsyncS::event_loop()
{
    std::unique_lock<std::mutex> lk(m);
    while (running)
    {
        cv.wait(lk);

        if (!m_event_queue.empty())
        {
            // on a different thread!
            ++asyncs;
            execute(std::move(m_event_queue.back()));
            m_event_queue.pop();
        }

        if (m_callback_queue.empty() and m_event_queue.empty() and asyncs == 0)
        {
            running = 0;
        }

        lk.unlock();
    }
    m_eval->reset_async_flag();
}

void AsyncS::execute(detail::Callback call)
{
    call();
    m_callback_queue.push(432);
    m_eval->callback_cv.notify_all();
    --asyncs;
}

void AsyncS::eval_callback(detail::Callback)
{


    std::cout << "submiting to the eval"
              << "\n";
    // submit to main thread here
}

void AsyncS::submit(detail::Callback t_callback)
{
    std::lock_guard<std::mutex> lk(m);
    running = 1;
    m_event_queue.push(std::move(t_callback));
    std::cout << "submiting"
              << "\n";
    cv.notify_all();
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
