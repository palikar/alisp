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

#include <atomic>
#include <csignal>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_asyncs.hpp"

namespace alisp
{

namespace eval
{

namespace detail
{
class EvalDepthTrack;
class CatchTrack;

}  // namespace detail

class Evaluator
{
  private:
    env::Environment &env;
    size_t m_eval_depth;
    size_t m_catching_depth;
    parser::ParserBase *m_parser;

    async::AsyncS m_async;

    int m_signal;

    std::atomic_uint_fast32_t m_status_flags;

    std::string m_current_file;

    static constexpr std::uint32_t SIGINT_FLAG            = 0x0001;
    static constexpr std::uint32_t ACTIVE_EVALUATION_FLAG = 0x0002;
    static constexpr std::uint32_t SIGTERM_FLAG           = 0x0004;
    static constexpr std::uint32_t ASYNC_FLAG             = 0x0008;
    static constexpr std::uint32_t INTERACTIVE_FLAG       = 0x0010;


  public:
    std::mutex callback_m;
    std::unique_lock<std::mutex> m_lock;
    std::condition_variable callback_cv;
    std::condition_variable futures_cv;

    Evaluator(env::Environment &env_, parser::ParserBase *t_parser, bool t_defer_el = false);
    ~Evaluator();


    void eval_file(const std::string &t_file);
    void eval_string(std::string &t_eval);

    ALObjectPtr eval(ALObjectPtr obj);
    ALObjectPtr eval_function(ALObjectPtr func, ALObjectPtr args);
    ALObjectPtr apply_function(ALObjectPtr func, ALObjectPtr args);
    ALObjectPtr handle_lambda(ALObjectPtr func, ALObjectPtr args);


    template<bool evaluation = true> void handle_argument_bindings(ALObjectPtr params, ALObjectPtr args);
    void put_argument(ALObjectPtr param, ALObjectPtr arg);

    void new_evaluation();
    void end_evaluation();

    size_t evaluation_depth() const { return m_eval_depth; }

    void handle_signal(int t_c);

    void check_status();

    void lock_evaluation();
    void unlock_evaluation();

    inline std::unique_lock<std::mutex> &lock() { return m_lock; }

    void inline set_evaluation_flag() { m_status_flags |= ACTIVE_EVALUATION_FLAG; }
    void inline reset_evaluation_flag() { m_status_flags &= ~ACTIVE_EVALUATION_FLAG; }

    void inline set_async_flag() { m_status_flags |= ASYNC_FLAG; }
    void inline reset_async_flag() { m_status_flags &= ~ASYNC_FLAG; }
    bool inline is_async_pending() { return (m_status_flags & ASYNC_FLAG) > 0; }

    void inline set_interactive_flag() { m_status_flags |= INTERACTIVE_FLAG; }
    void inline reset_interactive_flag() { m_status_flags &= ~INTERACTIVE_FLAG; }
    bool inline is_interactive() { return (m_status_flags & INTERACTIVE_FLAG) > 0; }

    void dispatch_callbacks();

    void set_current_file(std::string t_tile);
    const std::string &get_current_file();

    async::AsyncS &async() { return m_async; }

    friend detail::EvalDepthTrack;
    friend detail::CatchTrack;
};

namespace detail
{

class EvalDepthTrack
{
  public:
    explicit EvalDepthTrack(Evaluator &t_eval);
    ~EvalDepthTrack();

    ALISP_RAII_OBJECT(EvalDepthTrack);

  private:
    Evaluator &m_eval;
};

class CatchTrack
{
  public:
    explicit CatchTrack(Evaluator &t_eval);
    ~CatchTrack();

    ALISP_RAII_OBJECT(CatchTrack);

  private:
    Evaluator &m_eval;
};

class EvaluationLock
{
  public:
    explicit EvaluationLock(Evaluator &t_eval);
    ~EvaluationLock();

    ALISP_RAII_OBJECT(EvaluationLock);

  private:
    Evaluator &m_eval;
};

}  // namespace detail

}  // namespace eval

}  // namespace alisp
