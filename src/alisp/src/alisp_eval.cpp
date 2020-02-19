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


#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_factory.hpp"

#include "alisp/utility.hpp"

#include <algorithm>

namespace alisp
{

namespace eval
{


void Evaluator::new_evaluation()
{
    ++m_eval_depth;
    if (m_eval_depth > MAX_EAVALUATION_DEPTH) { throw eval_error("Maximum evaluation depth reached!"); }
}

void Evaluator::end_evaluation()
{
    --m_eval_depth;
}

Evaluator::Evaluator(env::Environment &env_, parser::ParserBase *t_parser) : env(env_), m_eval_depth(0), m_parser(t_parser), m_status_flags(0)
{
}

void Evaluator::put_argument(ALObjectPtr param, ALObjectPtr arg)
{
    AL_DEBUG("Putting argument: "s += dump(param) + " -> " + dump(arg));

    this->env.put(param, arg);
}

template<bool evaluation> void Evaluator::handle_argument_bindings(ALObjectPtr params, ALObjectPtr args)
{

    if (params->length() == 0 && args->length() != 0) { throw argument_error("Argument\'s lengths do not match."); }

    if (args->length() != 0 && params->length() == 0) { throw argument_error("Argument\'s lengths do not match."); }

    if (args->length() == 0 && params->length() == 0) { return; }


    auto eval_args = args;

    auto next_argument = std::begin(*eval_args);
    auto next_param    = std::begin(*params);


    auto end_param = std::end(*params);

    auto arg_cnt = static_cast<ALObject::list_type::difference_type>(args->length());

    ALObject::list_type::difference_type index = 0;
    bool rest                                  = false;
    bool opt                                   = false;
    bool prev_opt_or_rest                      = false;

    while (next_param != end_param)
    {
        if (*next_param == Qoptional)
        {
            opt              = true;
            prev_opt_or_rest = true;
            next_param       = std::next(next_param);
            continue;
        }
        else if (*next_param == Qrest)
        {
            rest             = true;
            prev_opt_or_rest = true;
            next_param       = std::next(next_param);
            continue;
        }
        else
        {

            if (rest)
            {
                put_argument(*next_param, splice(eval_args, index));
                return;
            }
            else if (index < arg_cnt)
            {
                put_argument(*next_param, *next_argument);
            }
            else if (!opt)
            {
                throw argument_error("The function requires more arguments than the provided ones.");
            }
            else
            {
                put_argument(*next_param, Qnil);
            }

            ++index;
            prev_opt_or_rest = false;
            next_argument    = std::next(next_argument);
            next_param       = std::next(next_param);
        }
    }


    if (prev_opt_or_rest) { throw argument_error("The argument list ends with &optional or &rest."); }

    if (index < arg_cnt) { throw argument_error("Too many arguments provided for the function call."); }
}

ALObjectPtr Evaluator::eval(ALObjectPtr obj)
{
    detail::EvalDepthTrack track{ *this };

    if (is_falsy(obj)) return obj;

    switch (obj->type())
    {
    case ALObjectType::STRING_VALUE:

    case ALObjectType::REAL_VALUE:

    case ALObjectType::INT_VALUE:
    {
        return obj;
    }

    case ALObjectType::SYMBOL:
    {
        AL_DEBUG("Evaluating symbol: "s += dump(obj));
        return env.find(obj);
    }

    case ALObjectType::LIST:
    {

        auto func = eval(obj->i(0));

        if (psym(func)) { func = env.find(func); }

        if (!func->check_function_flag()) { throw eval_error("Head of a list must be bound to function"); }

#ifdef ENABLE_STACK_TRACE

        env::detail::CallTracer tracer{ env };

        if (obj->prop_exists("--line--")) { tracer.line(obj->get_prop("--line--")->to_int()); }

        if (func->prop_exists("--name--")) { tracer.function_name(func->get_prop("--name--")->to_string(), func->check_prime_flag()); }
        else
        {
            tracer.function_name("anonymous", false);
        }

#endif
        AL_DEBUG("Calling funcion: "s += dump(obj->i(0)));

        try
        {

            if (func->check_prime_flag())
            {

                STACK_ALLOC_OBJECT(eval_obj, eval_ptr, utility::slice_view(obj->children(), 1));

                return func->get_prime()(eval_ptr, &env, this);
            }
            else if (func->check_macro_flag())
            {

                env::detail::MacroCall fc{ env };

                STACK_ALLOC_OBJECT(eval_obj, eval_ptr, utility::slice_view(obj->children(), 1));
                auto a = apply_function(func, eval_ptr);
                AL_DEBUG("Macro expansion: "s += dump(a));
                return eval(a);
            }
            else
            {
                STACK_ALLOC_OBJECT(eval_obj, eval_ptr, utility::slice_view(obj->children(), 1));

                return eval_function(func, eval_ptr);
            }
        }
        catch (al_continue &)
        {
            throw;
        }
        catch (al_break &)
        {
            throw;
        }
        catch (al_exit &)
        {
            throw;
        }
        catch (al_return &)
        {
            throw;
        }
        catch (interrupt_error &)
        {
            throw;
        }
        catch (...)
        {
#ifdef ENABLE_STACK_TRACE
            tracer.dump();
#endif
            throw;
        }


        break;
    }

    default:
    {
        eval_error("Unknown object typee");
    }
    }

    return nullptr;
}

ALObjectPtr Evaluator::eval_function(ALObjectPtr func, ALObjectPtr args)
{
    auto [params, body] = func->get_function();
    auto eval_args      = eval_transform(this, args);
    try
    {
        if (!plist(body) || std::size(*body) == 0) { return Qnil; }
        env::detail::FunctionCall fc{ env, func };
        handle_argument_bindings(params, eval_args);
        return eval_list(this, body, 0);
    }
    catch (al_return &ret)
    {
        return ret.value();
    }
}

ALObjectPtr Evaluator::apply_function(ALObjectPtr func, ALObjectPtr args)
{
    auto [params, body] = func->get_function();
    handle_argument_bindings<false>(params, args);
    return eval_list(this, body, 0);
}

ALObjectPtr Evaluator::handle_lambda(ALObjectPtr func, ALObjectPtr args)
{
    AL_DEBUG("Calling lambda: "s += dump(func));

    auto obj = func;
    if (psym(func)) { obj = eval(func); }

    if (!obj->check_function_flag()) { throw eval_error("Cannot apply a non function object."); }

    env::detail::FunctionCall fc{ env, func };
    if (obj->check_prime_flag()) { return obj->get_prime()(args, &env, this); }
    else
    {
        return apply_function(obj, args);
    }
}

void Evaluator::eval_file(const std::string &t_file)
{
    AL_DEBUG("Evaluating file: "s += t_file);

    auto file_content = utility::load_file(t_file);

    auto parse_result = m_parser->parse(file_content, t_file);

    for (auto sexp : parse_result) { eval(sexp); }
}

void Evaluator::eval_string(std::string &t_eval)
{
    AL_DEBUG("Evaluating string: "s += t_file);

    auto parse_result = m_parser->parse(t_eval, "--EVAL--");

    for (auto sexp : parse_result) { eval(sexp); }
}

void Evaluator::handle_signal(int t_c)
{
    if (t_c == SIGINT)
    {
        AL_DEBUG("Handling a SIGINT"s);

        if ((m_status_flags & ACTIVE_EVALUATION_FLAG) == 0)
        {
            throw interrupt_error();
            return;
        }

        m_signal = t_c;
        m_status_flags |= SIGINT_FLAG;
    }
}

void Evaluator::set_evaluation_flag()
{
    m_status_flags |= ACTIVE_EVALUATION_FLAG;
}

void Evaluator::reset_evaluation_flag()
{
    m_status_flags &= ~ACTIVE_EVALUATION_FLAG;
}

void Evaluator::check_status()
{
    if ((m_status_flags & SIGINT_FLAG) > 0)
    {
        m_status_flags &= ~SIGINT_FLAG;
        throw interrupt_error();
    }
}

detail::EvalDepthTrack::EvalDepthTrack(Evaluator &t_eval) : m_eval(t_eval)
{
    m_eval.check_status();
    m_eval.new_evaluation();
    m_eval.set_evaluation_flag();
}

detail::EvalDepthTrack::~EvalDepthTrack()
{
    m_eval.end_evaluation();
    if (m_eval.evaluation_depth() == 0) { m_eval.reset_evaluation_flag(); }
}

}  // namespace eval

}  // namespace alisp
