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

    int m_signal;
    std::uint_fast32_t m_status_flags;

    std::string m_current_file;

    static constexpr std::uint32_t SIGINT_FLAG            = 0x0001;
    static constexpr std::uint32_t ACTIVE_EVALUATION_FLAG = 0x0002;


  public:
    Evaluator(env::Environment &env_, parser::ParserBase *t_parser);

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
    void set_evaluation_flag();
    void reset_evaluation_flag();

    void set_current_file(std::string t_tile);
    const std::string &get_current_file();

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

    EvalDepthTrack(EvalDepthTrack &&) = default;
    EvalDepthTrack &operator=(EvalDepthTrack &&) = default;
    EvalDepthTrack(const EvalDepthTrack &)       = delete;
    EvalDepthTrack &operator=(const EvalDepthTrack &) = delete;

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

}  // namespace detail

}  // namespace eval

}  // namespace alisp
