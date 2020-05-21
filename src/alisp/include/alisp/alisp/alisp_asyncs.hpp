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

namespace detail
{

template<typename T>
void pop_front(std::vector<T> &v)
{
    if (v.size() > 0) {
        v.erase(v.begin());
    }
}

struct AbstractCallback
{
    virtual void call() const = 0;
    virtual ~AbstractCallback() = default;
};

template<class T>
struct WrappingCallback : AbstractCallback
{
    T cb_;
    explicit WrappingCallback(T && cb) : cb_(std::move(cb)) {}
    void call() const override { return cb_(); }
};

struct Callback
{
    std::unique_ptr<AbstractCallback> ptr_;

    template<class T>
    Callback(T t) {
        ptr_ = std::make_unique<WrappingCallback<T>>(std::move(t));
    }

    void operator()() const {
        ptr_->call();
    }
};


}

class AsyncS
{
  private:
    eval::Evaluator *m_eval;

    std::queue<detail::Callback> m_event_queue;
    std::queue<int> m_callback_queue;
    std::thread m_event_loop;
    std::atomic_int running;

    std::atomic_int asyncs{0};

    mutable std::mutex event_loop_mutex;
    mutable std::mutex queue_mutex;
    mutable std::condition_variable event_loop_cv;


    void event_loop();

    void execute(detail::Callback call);

    void eval_callback(detail::Callback);

  public:

    AsyncS(eval::Evaluator *t_eval);

    void submit(detail::Callback t_callback);

    void end();

    bool has_callback();
    
    int next_callback();    
};

}

}  // namespace alisp
