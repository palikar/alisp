/* MIT License

   Copyright (c) 2017 Robert Vaser

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. */


#include <exception>
#include <stdexcept>

#include "alisp/alisp/async/thread_pool.hpp"

namespace alisp::async::thread_pool
{

Semaphore::Semaphore(std::uint32_t value) : value_(value)
{
}

void Semaphore::post()
{
    std::unique_lock<std::mutex> lock(mutex_);
    ++value_;
    condition_.notify_one();
}

void Semaphore::wait()
{
    std::unique_lock<std::mutex> lock(mutex_);
    condition_.wait(lock, [&]() { return value_; });
    --value_;
}

ThreadPool::ThreadPool(std::uint32_t num_threads) : queue_sem_{ 1 }, active_sem_{ 0 }
{

    terminate_ = false;
    for (std::uint32_t i = 0; i < num_threads; ++i)
    {
        threads_.emplace_back(ThreadPool::worker_thread, this);
    }
}

ThreadPool::~ThreadPool()
{

    terminate_ = true;
    for (std::uint32_t i = 0; i < threads_.size(); ++i)
    {
        active_sem_.post();
    }
    for (auto &it : threads_)
    {
        it.join();
    }
}

void ThreadPool::worker_thread(ThreadPool *thread_pool)
{

    while (true)
    {
        thread_pool->active_sem_.wait();

        if (thread_pool->terminate_)
        {
            break;
        }

        thread_pool->queue_sem_.wait();

        auto task = std::move(thread_pool->task_queue_.front());
        thread_pool->task_queue_.pop();

        thread_pool->queue_sem_.post();

        if (thread_pool->terminate_)
        {
            break;
        }

        task();
    }
}

}  // namespace alisp::async::thread_pool
