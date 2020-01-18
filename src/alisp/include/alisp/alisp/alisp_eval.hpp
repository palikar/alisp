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

namespace alisp
{

namespace eval
{

class Evaluator
{
  private:
    env::Environment &env;
    size_t m_eval_depth = 0;
    std::shared_ptr<parser::ParserBase> m_parser;

  public:
    Evaluator(env::Environment &env_, std::shared_ptr<parser::ParserBase> t_parser);

    void eval_file(const std::string& t_file);

    ALObjectPtr eval(ALObjectPtr obj);
    ALObjectPtr eval_function(ALObjectPtr func, ALObjectPtr args);
    ALObjectPtr apply_function(ALObjectPtr func, ALObjectPtr args);
    ALObjectPtr handle_lambda(ALObjectPtr func, ALObjectPtr args);
    

    template<bool evaluation = true>
    void handle_argument_bindings(ALObjectPtr params, ALObjectPtr args);
    void put_argument(ALObjectPtr param, ALObjectPtr arg);

    void new_evaluation();
    void end_evaluation();
    
};

namespace detail
{

class EvalDepthTrack
{
  public:

    explicit EvalDepthTrack(Evaluator& t_eval) : m_eval(t_eval) {m_eval.new_evaluation();}
    ~EvalDepthTrack() { m_eval.end_evaluation();}

    EvalDepthTrack(EvalDepthTrack &&) = default;
    EvalDepthTrack& operator=(EvalDepthTrack &&) = default;
    EvalDepthTrack(const EvalDepthTrack &) = delete;
    EvalDepthTrack& operator=(const EvalDepthTrack  &) = delete;

  private:
    Evaluator& m_eval;

};

}

}

}
