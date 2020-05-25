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

namespace eval
{
class Evaluator;
}

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
};

class AsyncS;

namespace detail
{

struct AbstractCallback
{
    virtual ALObjectPtr call(AsyncS *async) const = 0;
    virtual ~AbstractCallback()                   = default;
};

template<class T> struct WrappingCallback : AbstractCallback
{
    T cb_;
    explicit WrappingCallback(T &&cb) : cb_(std::move(cb)) {}
    ALObjectPtr call(AsyncS *async) const override { return cb_(async); }
};

struct EventObject
{
    std::unique_ptr<AbstractCallback> ptr_;

    Future *future;

    template<class T> EventObject(T t) { ptr_ = std::make_unique<WrappingCallback<T>>(std::move(t)); }

    ALObjectPtr operator()(AsyncS *async) const { return ptr_->call(async); }
};

struct CallbackObject
{
    ALObjectPtr function;
    ALObjectPtr arguments;

    std::function<void(ALObjectPtr)> internal{};
};

}  // namespace detail

class AsyncS
{
  public:
    static constexpr size_t POOL_SIZE = 3;
    using callback_type               = detail::CallbackObject;
    using event_type                  = detail::EventObject;

    static constexpr std::uint32_t RUNNING_FLAG     = 0x0001;
    static constexpr std::uint32_t EL_SPINNING_FLAG = 0x0002;
    static constexpr std::uint32_t INIT_FLAG        = 0x0004;

    inline static management::Registry<Future, 0x05> futures{};

  private:
    eval::Evaluator *m_eval;

    std::queue<event_type> m_event_queue;
    std::queue<callback_type> m_callback_queue;
    std::thread m_event_loop;
    std::atomic_uint32_t m_flags;


    std::atomic_int m_asyncs{ 0 };

    mutable std::mutex callback_queue_mutex;
    mutable std::mutex future_mutex;


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

    void execute_event(event_type call);

    void init();

  public:
    AsyncS(eval::Evaluator *t_eval, bool defer_init = false);

    void submit_event(event_type t_event);

    void submit_callback(ALObjectPtr function, ALObjectPtr args = nullptr, std::function<void(ALObjectPtr)> internal = {});

    uint32_t new_future();

    void submit_future(uint32_t t_id, ALObjectPtr t_value, bool t_good = true);

    inline Future &future(uint32_t t_id) { return futures[t_id]; }

    ALObjectPtr future_resolved(uint32_t t_id);

    void spin_loop();

    void end();

    bool has_callback();

    callback_type next_callback();

    inline std::uint32_t status_flags() { return m_flags; }
};


template<typename T, typename... Args> auto dispatch(AsyncS &async, Args &&... args)
{

    if constexpr (T::managed)
    {
        T event_object{ std::forward<decltype(args)>(args)... };


        if constexpr (T::has_future)
        {
            auto fut = event_object.future(&async);
            async.submit_event(detail::EventObject{ std::move(event_object) });
            return fut;
        }
        else
        {
            async.submit_event(detail::EventObject{ std::move(event_object) });
            return Qt;
        }
    }
    else
    {
        T event_object{ std::forward<decltype(args)>(args)... };
        return event_object(&async);
    }
}

}  // namespace async

}  // namespace alisp
