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
#include "alisp/alisp/alisp_factory.hpp"

#include <chrono>




namespace alisp
{

namespace async
{


AsyncS::AsyncS(eval::Evaluator *t_eval) : m_eval(t_eval)
{
    m_running = 1;

#ifndef MULTI_THREAD_EVENT_LOOP
    m_event_loop = std::thread(&AsyncS::event_loop, this);

#else
    for (size_t i = 0; i < POOL_SIZE; ++i)
    {
        pool[i] = std::thread(&AsyncS::event_loop_thread, this);
    }
    
#endif
    
}

#ifndef MULTI_THREAD_EVENT_LOOP

void AsyncS::event_loop()
{
    
    using namespace std::chrono_literals;
    std::unique_lock<std::mutex> el_lock{event_loop_mutex, std::defer_lock};
    while (m_running)
    {
        event_loop_cv.wait(el_lock);

        if (m_running == 0)
        {
            break;
        }

        while (!m_event_queue.empty())
        {
            execute_event(std::move(m_event_queue.front()));
            ++m_asyncs;
            m_event_queue.pop();
        }
        
        
        if (m_callback_queue.empty() and m_event_queue.empty() and m_asyncs == 0)
        {
            m_running = 0;
            m_eval->reset_async_flag();
            m_eval->callback_cv.notify_all();
            return;
        }
    }
    
}

#else

void AsyncS::event_loop_thread()
{
    using namespace std::chrono_literals;
    std::unique_lock<std::mutex> th_lock(pool_mutex);

    while(m_running == 1)
    {
        pool_cv.wait(th_lock);

        if (m_running == 0)
        {
            break;
        }

        std::unique_lock<std::mutex> lock{event_queue_mutex};
        if (!m_event_queue.empty())
        {
            auto call = std::move(m_event_queue.front());
            m_event_queue.pop();
            lock.unlock();
                        
            ++m_asyncs;
            execute_event(std::move(call));
            
        }
        else
        {
            lock.unlock();
        }

        if (m_running == 1 and m_callback_queue.empty() and m_event_queue.empty() and m_asyncs == 0)
        {
            m_running = 0;
            m_eval->reset_async_flag();
            pool_cv.notify_all();
            m_eval->callback_cv.notify_all();
            break;
        }
                
    }

}
    
#endif

void AsyncS::execute_event(detail::Callback call)
{

    std::thread tr([&, call = std::move(call)]{
        call(this);
        --m_asyncs;
        m_eval->callback_cv.notify_all();       
    });
    
    tr.detach();
}

void AsyncS::submit_event(detail::Callback t_callback)
{
    {
        
#ifndef MULTI_THREAD_EVENT_LOOP
        std::lock_guard<std::mutex> guard{event_loop_mutex};
#else
        std::lock_guard<std::mutex> guard{event_queue_mutex};
#endif
        m_event_queue.push(std::move(t_callback));
        
        m_eval->set_async_flag();    
    }
    
#ifndef MULTI_THREAD_EVENT_LOOP
    event_loop_cv.notify_one();
#else
    pool_cv.notify_one();
#endif
}

void AsyncS::submit_callback(ALObjectPtr function, ALObjectPtr args)
{
    
    {
        std::lock_guard<std::mutex> guard(callback_queue_mutex);
        if (args == nullptr) {
            m_callback_queue.push({function, make_list()});
        }else{
            m_callback_queue.push({function, args});
        }            
    }
    
    m_eval->callback_cv.notify_all();
}

void AsyncS::spin_loop()
{
#ifndef MULTI_THREAD_EVENT_LOOP
    event_loop_cv.notify_one();
#else
    pool_cv.notify_all();
#endif
}

bool AsyncS::has_callback()
{
    std::lock_guard<std::mutex> guard(callback_queue_mutex);
    return !m_callback_queue.empty();
}

AsyncS::callback_type AsyncS::next_callback()
{
    std::lock_guard<std::mutex> guard(callback_queue_mutex);
    auto value = m_callback_queue.front();
    m_callback_queue.pop();
    event_loop_cv.notify_all();
    return value;
}

void AsyncS::end()
{
    m_running = 0;
    
#ifndef MULTI_THREAD_EVENT_LOOP
    if (m_event_loop.joinable())
    {
        m_event_loop.join();
    }

#else
    pool_cv.notify_all();
    for (size_t i = 0; i < 3; ++i)
    {
        if (pool[i].joinable())
        {
            pool[i].join();
        }
    }
    
#endif


}

}  // namespace async

}  // namespace alisp
