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


#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_asyncs.hpp"
#include "alisp/alisp/alisp_eval.hpp"

#include "alisp/alisp/async/timing.hpp"
#include "alisp/alisp/async/action.hpp"

namespace alisp
{

namespace detail
{

ALObjectPtr Fasync_start(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(obj));
    AL_CHECK(assert_max_size<2>(obj));

    auto action = eval->eval(obj->i(0));
    AL_CHECK(assert_function(action));

    auto callback = Qnil;
    if (std::size(*obj) > 1)
    {
        callback = eval->eval(obj->i(1));
        AL_CHECK(assert_function(callback));
    }

    auto res = async::dispatch<async_action>(eval->async(), std::move(action), std::move(callback));

    if (pint(res) and eval->async().futures.belong(object_to_resource(res)))
    {
        env->defer_callback([eval, id = object_to_resource(res)]() { eval->async().dispose_future(id); });
    }

    return res;
}

ALObjectPtr Fasync_await(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto future = eval->eval(obj->i(0));
    AL_CHECK(assert_int(future));


    {
        async::Await await{ eval->async() };

        eval->futures_cv.wait(eval->lock(),
                              [&] { return is_truthy(eval->async().future(object_to_resource(future)).resolved); });
    }

    return eval->async().future(object_to_resource(future)).value;
}

ALObjectPtr Fasync_then(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));
    AL_CHECK(assert_max_size<3>(obj));

    auto future           = eval->eval(obj->i(0));
    auto success_callback = eval->eval(obj->i(1));
    AL_CHECK(assert_int(future));
    AL_CHECK(assert_function(success_callback));

    auto reject_callback = Qnil;
    if (std::size(*obj) > 2)
    {
        reject_callback = eval->eval(obj->i(2));
        if (!pfunction(reject_callback))
        {
            reject_callback = Qnil;
        }
    }

    auto &fut = eval->async().future(object_to_resource(future));

    if (is_truthy(fut.resolved))
    {

        if (is_truthy(fut.success_state))
        {
            eval->eval_callable(fut.success_callback, make_list(fut.value));
        }
        else
        {
            eval->eval_callable(fut.reject_callback, make_list(fut.value));
        }
    }
    else
    {
        fut.success_callback = success_callback;
        fut.reject_callback  = reject_callback;
    }

    return Qt;
}

ALObjectPtr Fasync_ready(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));

    auto future = eval->eval(obj->i(0));
    AL_CHECK(assert_int(future));

    return eval->async().future(object_to_resource(future)).resolved;
}

ALObjectPtr Fasync_state(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));

    auto future = eval->eval(obj->i(0));
    AL_CHECK(assert_int(future));

    return eval->async().future(object_to_resource(future)).success_state;
}

ALObjectPtr Ftimeout(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto fun  = eval->eval(obj->i(0));
    auto time = eval->eval(obj->i(1));
    AL_CHECK(assert_int(time));
    AL_CHECK(assert_function(fun));

    async::dispatch<set_timeout>(eval->async(), static_cast<size_t>(time->to_int()), fun);

    return Qt;
}

}  // namespace detail

env::ModulePtr init_async(env::Environment *, eval::Evaluator *)
{

    auto Masync    = module_init("async");
    auto async_ptr = Masync.get();

    module_defun(async_ptr,
                 "timeout",
                 &detail::Ftimeout,
                 R"((set-timeout CALLBACK MILLISECONDS)

Execute `CALLBACK` after `SECONDS`.
)");

    module_defun(async_ptr, "async-start", &detail::Fasync_start);

    module_defun(async_ptr,
                 "async-then",
                 &detail::Fasync_then,
                 R"((future-then FUTURE SUCCESS REJECT)

)");

    module_defun(async_ptr,
                 "async-await",
                 &detail::Fasync_await,
                 R"((future-await FUTURE)

Block the main thread till `FUTURE` is complete and return return the
value of the future.
)");

    module_defun(async_ptr,
                 "async-ready",
                 &detail::Fasync_ready,
                 R"((async-ready FUTURE)

)");

    module_defun(async_ptr,
                 "async-state",
                 &detail::Fasync_state,
                 R"((async-state FUTURE)

)");


    return Masync;
}


}  // namespace alisp
