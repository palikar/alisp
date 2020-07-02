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

struct async_start
{
    static inline const std::string name{ "async-start" };

    static inline const std::string doc{ R"()" };

    static inline const Signature signature{ Function{}, Optional{}, Function{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
    {

        auto action = arg_eval(eval, obj, 0);

        auto callback = Qnil;
        if (std::size(*obj) > 1)
        {
            callback = arg_eval(eval, obj, 1);
        }

        auto res = async::dispatch<async_action>(eval->async(), std::move(action), std::move(callback));

        if (pint(res) and eval->async().futures.belong(object_to_resource(res)))
        {
            env->defer_callback([eval, id = object_to_resource(res)]() { eval->async().dispose_future(id); });
        }

        return res;
    }
};

struct async_await
{
    static inline const std::string name{ "async-await" };

    static inline const std::string doc{ R"((future-then FUTURE SUCCESS REJECT))" };

    static inline const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto future = arg_eval(eval, obj, 0);


        {
            async::Await await{ eval->async() };

            eval->futures_cv.wait(eval->lock(),
                                  [&] { return is_truthy(eval->async().future(object_to_resource(future)).resolved); });
        }

        return eval->async().future(object_to_resource(future)).value;
    }
};

struct async_then
{
    static inline const std::string name{ "async-then" };

    static inline const std::string doc{ R"((async-ready FUTURE)

)" };

    static inline const Signature signature{ Int{}, Function{}, Optional{}, Function{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {

        auto future           = arg_eval(eval, obj, 0);
        auto success_callback = arg_eval(eval, obj, 1);

        auto reject_callback = Qnil;
        if (std::size(*obj) > 2)
        {
            reject_callback = arg_eval(eval, obj, 2);
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

        return future;
    }
};

struct async_ready
{
    static inline const std::string name{ "async-ready" };

    static inline const std::string doc{ R"((async-state FUTURE)

)" };

    static inline const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {

        auto future = arg_eval(eval, obj, 0);

        return eval->async().future(object_to_resource(future)).resolved;
    }
};

struct async_state
{
    static inline const std::string name{ "async-state" };

    static inline const std::string doc{ R"()" };

    static inline const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {

        auto future = arg_eval(eval, obj, 0);

        return eval->async().future(object_to_resource(future)).success_state;
    }
};

struct timeout
{
    static inline const std::string name{ "timeout" };

    static inline const std::string doc{ R"((async-state FUTURE)

)" };

    static inline const Signature signature{ Int{}, Function{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto fun  = arg_eval(eval, obj, 0);
        auto time = arg_eval(eval, obj, 1);

        async::dispatch<set_timeout>(eval->async(), static_cast<size_t>(time->to_int()), fun);

        return Qt;
    }
};

struct module_doc
{

    static inline const std::string doc{ R"((async-state FUTURE)

)" };
};

}  // namespace detail

env::ModulePtr init_async(env::Environment *, eval::Evaluator *)
{

    auto Masync    = module_init("async");
    auto async_ptr = Masync.get();

    using namespace detail;

    module_defun(async_ptr, async_start::name, async_start::func, async_start::doc, async_start::signature.al());
    module_defun(async_ptr, async_await::name, async_await::func, async_await::doc, async_await::signature.al());
    module_defun(async_ptr, async_then::name, async_then::func, async_then::doc, async_then::signature.al());
    module_defun(async_ptr, async_ready::name, async_ready::func, async_ready::doc, async_ready::signature.al());
    module_defun(async_ptr, async_state::name, async_state::func, async_state::doc, async_state::signature.al());
    module_defun(async_ptr, timeout::name, timeout::func, timeout::doc, timeout::signature.al());


    return Masync;
}


}  // namespace alisp
