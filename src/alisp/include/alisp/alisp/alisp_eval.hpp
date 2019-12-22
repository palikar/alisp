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

  public:
    Evaluator(env::Environment &env_);

    ALObject* eval(ALObject* obj);
    ALObject* eval_function(ALObject* func, ALObject* args);
    ALObject* apply_function(ALObject* func, ALObject* args);
    ALObject* handle_lambda(ALObject* func, ALObject* args);
    

    template<bool evaluation = true>
    void handle_argument_bindings(ALObject* params, ALObject* args);
    void put_argument(ALObject* param, ALObject* arg);

    void new_evaluation() {
        ++m_eval_depth;
        if (m_eval_depth > MAX_EAVALUATION_DEPTH) { throw std::runtime_error("Maximum evaluation depth reached!"); }
    }
    
    void end_evaluation() { --m_eval_depth; }

    
};

namespace detail {

class EvalDepthTrack {
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
