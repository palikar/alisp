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


AsyncS::AsyncS(eval::Evaluator *t_eval) : m_eval(t_eval)// , m_thread_pool(2)
{
    m_event_loop = std::thread(&AsyncS::event_loop, this);
    m_running = 1;

    
    for (size_t i = 0; i < POOL_SIZE; ++i) {
        
        auto la = [&](){
            std::unique_lock<std::mutex> th_lock(pool_mutex);

            while(m_running == 1)
            {
                pool_cv.wait(th_lock);

                if (m_running == 0)
                {
                    break;
                }

                {
                    std::unique_lock<std::mutex> lock{event_queue_mute, std::defer_lock};
                    lock.lock();
                    if (!m_event_queue.empty())
                    {
                        auto call = std::move(m_event_queue.back());
                        m_event_queue.pop();
                        lock.unlock();
                        
                        call();
                        m_callback_queue.push(432);
                        --m_asyncs;
                        m_eval->callback_cv.notify_all();
                    }
                    else
                    {
                        lock.unlock();
                    }
                }
                
            }
            
        };
        
        pool[i] = std::thread(la);
    }

    
}

void AsyncS::event_loop()
{
    using namespace std::chrono_literals;
    std::unique_lock<std::mutex> el_lock{event_loop_mutex, std::defer_lock};
    while (m_running)
    {
        event_loop_cv.wait_for(el_lock, 100ms);

        if (m_running == 0)
        {
            break;
        }

        if (!m_event_queue.empty())
        {

            for (size_t i = 0; i < m_event_queue.size(); ++i)
            {
                if (m_asyncs < 3) {
                    ++m_asyncs;
                    pool_cv.notify_one();
                }

            }
            
            // execute(std::move(m_event_queue.back()));
            // m_event_queue.pop();
        }
        
        
        if (m_callback_queue.empty() and m_event_queue.empty() and m_asyncs == 0)
        {
            m_running = 0;
            break;
        }

    }

    m_eval->reset_async_flag();
    m_eval->callback_cv.notify_all();
}

void AsyncS::execute(detail::Callback call)
{

    std::thread tr(&AsyncS::execute_in_thread, this, std::move(call));
    tr.detach();
}


void AsyncS::execute_in_thread(detail::Callback call)
{
    call.ptr_.get()->call();
    m_callback_queue.push(432);
    --m_asyncs;
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
    m_running = 0;
    pool_cv.notify_all();
    if (m_event_loop.joinable())
    {
        m_event_loop.join();
    }
    
    for (size_t i = 0; i < 3; ++i)
    {
        if (pool[i].joinable())
        {
            pool[i].join();
        }
    }


}

}  // namespace async

}  // namespace alisp
