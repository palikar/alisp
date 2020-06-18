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

#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_env.hpp"


namespace func
{
using namespace alisp;

namespace details
{

}

auto placeholder_sym = alisp::make_symbol("_");

ALObjectPtr Fcompose(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));

    auto fun_fin = obj->i(std::size(*obj) - 1);
    auto res     = make_object(env::intern("apply"), fun_fin, env::intern("rest--"));

    for (size_t i = std::size(*obj) - 2; i != 0; --i)
    {
        res = make_object(obj->i(i), res);
    }
    res = make_object(obj->i(0), res);

    return Flambda(make_object(make_object(Qrest, env::intern("rest--")), res), env, eval);
}

ALObjectPtr Fpartial(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(obj));

    auto fun = obj->i(0);

    ALObject::list_type args_list;
    ALObject::list_type op_list;

    op_list.push_back(fun);

    size_t j = 0;
    for (size_t i = 1; i < std::size(*obj); ++i)
    {
        auto o = eval->eval(obj->i(i));

        if (placeholder_sym == o)
        {
            const auto s = std::string(1, char('a' + j++)) += "--";
            args_list.push_back(env::intern(s));
            op_list.push_back(env::intern(s));
            continue;
        }

        op_list.push_back(o);
    }

    args_list.push_back(Qrest);
    args_list.push_back(env::intern("rest--"));

    op_list.push_back(make_object(Qcomma_at, env::intern("rest--")));

    return Flambda(
      make_object(make_list(args_list),
                  make_object(env::intern("eval"), make_object(env::intern("backquote"), make_list(op_list)))),
      env,
      eval);
}

ALObjectPtr Fthread_first(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<2>(obj);

    auto first = obj->i(0);

    const size_t size = std::size(*obj);

    for (size_t i = 1; i < size; ++i)
    {
        auto next_ls = obj->i(i);
        ALObject::list_type l;
        l.push_back(next_ls->i(0));
        l.push_back(first);
        for (size_t j = 1; j < std::size(*next_ls); ++j)
        {
            l.push_back(next_ls->i(j));
        }
        first = make_list(l);
    }

    return eval->eval(first);
}

ALObjectPtr Fthread_last(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<2>(obj);

    auto first = obj->i(0);

    const size_t size = std::size(*obj);

    for (size_t i = 1; i < size; ++i)
    {
        auto next_ls = obj->i(i);
        ALObject::list_type l;
        l.push_back(next_ls->i(0));
        for (size_t j = 1; j < std::size(*next_ls); ++j)
        {
            l.push_back(next_ls->i(j));
        }
        l.push_back(first);
        first = make_list(l);
    }
    return eval->eval(first);
}

ALObjectPtr Freduce(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));

    auto fun = eval->eval(obj->i(0));
    AL_CHECK(assert_function(fun));

    auto list = eval->eval(obj->i(1));
    AL_CHECK(assert_list(list));

    const size_t size = std::size(*list);

    if (size < 2)
    {
        return Qnil;
    }

    auto res = list->i(0);
    for (size_t i = 1; i < size; ++i)
    {
        res = eval->handle_lambda(fun, make_object(res, list->i(i)));
    }

    return res;
}

ALObjectPtr Fidentity(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<1>(obj));
    return obj->i(0);
}

ALObjectPtr Fignore(const ALObjectPtr&, env::Environment *, eval::Evaluator *)
{
    return Qnil;
}

}  // namespace func

ALISP_EXPORT alisp::env::ModulePtr init_func(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mfunc   = alisp::module_init("func");
    auto fun_ptr = Mfunc.get();

    alisp::module_doc(fun_ptr, R"( The `func` modules provides support for working with higher order
functions. It aims to bring more "functional" features to alisp.
)");


    alisp::module_defvar(
      fun_ptr, "_", func::placeholder_sym, R"(Can be used as a placeholder object at certain places.)");

    alisp::module_defun(fun_ptr,
                        "compose",
                        &func::Fcompose,
                        R"((compose [FUNCTION]...)

Create a new function by composing several ones. The last function
will be the innter most funciton in the composition.

Example:
```elisp
((compose (lambda (x) (* 2 x)) (lambda (x) (* 3 x)) ) 10) ; -> 60
```

In the above example, the compose function will return a function that
is equivalent to `(lambda (x) ((lambda (x_2) (* 2 x_2) ) ((lambda (x_1) (* 3 x_1)) x)))`.
This measns that the last function will be evalued first and then the
result of that will be used as input for the next function.
)");

    alisp::module_defun(fun_ptr,
                        "partial",
                        &func::Fpartial,
                        R"((partial FUNCTION [ARGUMENT] ...)

Create a new function by partially applying arguments to a
function. The return function can be called normally, either without
arguments (if every argument was partially applied) or with the
unapplied arguments.

Example:
```elisp
((partial (lambda (x y) (x + y)) 5) 2) ; -> 7
```
In the example, `(partial (lambda (x y) (x + y)) 5)` is equivalent to
`(lambda (x) ((lambda (x y) (x + y)) x 5))`. This is a function that 
takes a single argument and adds 5 to it.


 )");

    alisp::module_defun(fun_ptr,
                        "thread-last",
                        &func::Fthread_last,
                        R"((thread-last FORMS)

Thread FORMS elements as the last argument of their successor.

Example:
```elisp
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
```

Is equivalent to: `(+ 40 (- (/ 25 (+ 20 5))))`)");

    alisp::module_defun(fun_ptr,
                        "thread-first",
                        &func::Fthread_first,
                        R"((thread-first FORMS)

Thread FORMS elements as the first argument of their successor.

Example:
```elisp
(thread-first
      5
      (+ 20)
      (/ 25)
      (-)
      (+ 40))
```

Is equivalent to: `(+ (- (/ (+ 5 20) 25)) 40)` )");


    alisp::module_defun(fun_ptr,
                        "reduce",
                        &func::Freduce,
                        R"((reduce FUNCTION LIST)

Apply function of two arguments cumulatively to the items of LIST,
from left to right, so as to reduce the iterable to a single value.The
left argument is the accumulated value and the right argument is the
update value from the list.

```elisp
(reduce (lambda (x y) (+ x y)) '(1 2 3 4 5)) ; -> 15
```
 )");

    alisp::module_defun(fun_ptr,
                        "identity",
                        &func::Fidentity,
                        R"((identity ARG)

Return ARG unchanged.
)");

    alisp::module_defun(fun_ptr,
                        "ignore",
                        &func::Fignore,
                        R"((ignore [ANY]...)

Return nil and ignore all of the given arguments.
)");


    return Mfunc;
}
