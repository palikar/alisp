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

#pragma once

#include <cstdint>
#include <memory>
#include <vector>
#include <string>
#include <queue>
#include <mutex>
#include <thread>
#include <future>
#include <atomic>
#include <functional>
#include <condition_variable>

namespace alisp::async::thread_pool
{


class Semaphore
{
  public:
    Semaphore(std::uint32_t value);
    Semaphore(const Semaphore &) = delete;
    const Semaphore &operator=(const Semaphore &) = delete;
    ~Semaphore()                                  = default;

    std::uint32_t value() const { return value_; }

    void wait();
    void post();

  private:
    std::mutex mutex_;
    std::condition_variable condition_;
    std::uint32_t value_;
};

class ThreadPool
{
  public:
    ThreadPool(std::uint32_t num_threads);
    ThreadPool(const ThreadPool &) = delete;
    const ThreadPool &operator=(const ThreadPool &) = delete;
    ~ThreadPool();

    size_t num_threads() const { return threads_.size(); }

    template<typename T, typename... Ts> void submit(T &&routine, Ts &&... params)
    {

        auto task = [func = std::forward<T>(routine), args = std::make_tuple(std::forward<Ts>(params)...)]() {
            std::apply(func, args);
        };

        queue_sem_.wait();

        task_queue_.emplace(task);

        queue_sem_.post();
        active_sem_.post();
    }

  private:
    static void worker_thread(ThreadPool *thread_pool);

    std::vector<std::thread> threads_;

    std::queue<std::function<void()>> task_queue_;

    Semaphore queue_sem_;
    Semaphore active_sem_;

    std::atomic<bool> terminate_;
};

}  // namespace alisp::async::thread_pool
