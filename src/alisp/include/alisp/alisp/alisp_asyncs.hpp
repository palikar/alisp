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

namespace eval
{
class Evaluator;
}

namespace async
{

class AsyncS;
namespace detail
{

struct AbstractCallback
{
    virtual ALObjectPtr call(AsyncS *async) const = 0;
    virtual ~AbstractCallback() = default;
};

template<class T>
struct WrappingCallback : AbstractCallback
{
    T cb_;
    explicit WrappingCallback(T && cb) : cb_(std::move(cb)) {}
    ALObjectPtr call(AsyncS *async) const override { return cb_(async); }
};

struct Callback
{
    std::unique_ptr<AbstractCallback> ptr_;

    template<class T>
    Callback(T t) {
        ptr_ = std::make_unique<WrappingCallback<T>>(std::move(t));
    }

    ALObjectPtr operator()(AsyncS *async) const {
        return ptr_->call(async);
    }
};


}

class AsyncS
{
  public:
    static constexpr size_t POOL_SIZE = 3;
    using callback_type = std::pair<ALObjectPtr, ALObjectPtr>;
    
  private:
    eval::Evaluator *m_eval;

    std::queue<detail::Callback> m_event_queue;
    std::queue<callback_type> m_callback_queue;
    std::thread m_event_loop;
    std::atomic_int m_running;

    std::atomic_int m_asyncs{0};

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

    void execute_event(detail::Callback call);

  public:

    AsyncS(eval::Evaluator *t_eval);

    void submit_event(detail::Callback t_callback);

    void submit_callback(ALObjectPtr function, ALObjectPtr args = nullptr);

    void spin_loop();

    void end();

    bool has_callback();
    
    callback_type next_callback();    
};



template<typename T, typename ... Args>
auto dispatch(AsyncS &async, Args && ...  args)
{
    if constexpr(T::managed)
    {
        T event_object{std::forward<decltype(args)>(args) ... };
        async.submit_event(detail::Callback{std::move(event_object)});
    }
    else
    {
        T event_object{std::forward<decltype(args)>(args) ... };
        return event_object(async);
    }
    
}

}

}  // namespace alisp
