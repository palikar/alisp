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


#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/alisp/declarations/lists.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


struct Smapc
{
    inline static const std::string name = "mapc";

    inline static const std::string doc{ R"((mapc FUNCTION LIST)

Call FUNCTION for each element of LIST. The element is passed as an
argument to the function. `mapc` return `t` and its executed only for side effects.

Example:
```elisp
(mapc println '(1 2 3 4 5))
(mapc (lambda (x) (print x)) '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fmapc(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        auto fun_obj = eval->eval(obj->i(0));
        auto list    = eval->eval(obj->i(1));

        AL_CHECK(assert_function(fun_obj));

        if (plist(list))
        {
            for (auto &el : list->children())
            {
                if (psym(el) or plist(el))
                {
                    eval->eval_callable(fun_obj, make_list(quote(el)));
                }
                else
                {
                    eval->eval_callable(fun_obj, make_list(el));
                }
            }
            return Qt;
        }
        else if (pstring(list))
        {

            for (auto &el : list->to_string())
            {
                eval->eval_callable(fun_obj, make_list(make_char(el)));
            }
            return Qt;
        }


        return Qnil;
    }
};

struct Smapcar
{
    inline static const std::string name = "mapcar";

    inline static const std::string doc{ R"((mapcar FUNCTION LIST)


Call FUNCTION for each element of LIST while collecting the results of
the calls and building a new list. The new list is returned.

Example:
```elisp
(mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fmapcar(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        auto fun_obj = eval->eval(obj->i(0));
        auto list    = eval->eval(obj->i(1));


        AL_CHECK(assert_function(fun_obj));
        AL_CHECK(assert_list(list));

        ALObject::list_type new_l;

        for (auto &el : list->children())
        {

            if (psym(el) or plist(el))
            {
                new_l.push_back(eval->eval_callable(fun_obj, make_list(quote(el))));
            }
            else
            {
                new_l.push_back(eval->eval_callable(fun_obj, make_list(el)));
            }
        }

        return make_list(new_l);
    }
};

struct Scar
{
    inline static const std::string name = "mapcar";

    inline static const std::string doc{ R"((mapcar FUNCTION LIST)


Call FUNCTION for each element of LIST while collecting the results of
the calls and building a new list. The new list is returned.

Example:
```elisp
(mapcar (lambda (x) (+ x 1)) '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fcar(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto list = eval->eval(obj->i(0));
        return list->i(0);
    }
};

struct Scons
{
    inline static const std::string name = "cons";

    inline static const std::string doc{ R"((cons LIST)

Return a sublist of LIST with all of its elements but the first one.

Example:
```elisp
(cons '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fcons(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto list = eval->eval(obj->i(0));
        AL_CHECK(assert_list(list));
        return splice(list, 1);
    }
};

struct Scdr
{
    inline static const std::string name = "cdr";

    inline static const std::string doc{ R"((cdr LIST)

Return a sublist of LIST with all of its elements but the first one.

Example:
```elisp
(cdr '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fcdr(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        return splice(list, 1);
    }
};

struct Shead
{
    inline static const std::string name = "head";

    inline static const std::string doc{ R"((head LIST)

Return the fist element of the list LIST.

Example:
```elisp
(head '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fhead(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
    {
        return Scar::Fcar(obj, env, eval);
    }
};

struct Stail
{
    inline static const std::string name = "tail";

    inline static const std::string doc{ R"((mapc FUNCTION LIST)

Call FUNCTION for each element of LIST. The element is passed as an
argument to the function. `mapc` return `t` and its executed only for side effects.

Example:
```elisp
(mapc println '(1 2 3 4 5))
(mapc (lambda (x) (print x)) '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Ftail(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
    {
        return Scons::Fcons(obj, env, eval);
    }
};

struct Slast
{
    inline static const std::string name = "last";

    inline static const std::string doc{ R"((last LIST)

Return the last element of the list LIST.

Example:
```elisp
(last '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Flast(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        return list->i(list->length() - 1);
    }
};

struct Sinit
{
    inline static const std::string name = "init";

    inline static const std::string doc{ R"((init LIST)

Return a sublist of LIST with all of its elements but the last one.

Example:
```elisp
(init '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Finit(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        return splice(list, 0, static_cast<ALObject::list_type::difference_type>(list->length() - 1));
    }
};

struct Spush
{
    inline static const std::string name = "push";

    inline static const std::string doc{ R"((push LIST ELEMENT)

Add ELEMENT to the end of the LIST. This function changes the LIST
rather than to create a new one.

Example:
```elisp
(push '(1 2 3 4 5) 6)
```
)" };

    static ALObjectPtr Fpush(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        auto element = eval->eval(obj->i(1));

        list->children().push_back(element);

        return list;
    }
};

struct Sshove
{
    inline static const std::string name = "shove";

    inline static const std::string doc{ R"((shove LIST ELEMENT)

Add ELEMENT at the beginning of the LIST. This function changes the LIST
rather than to create a new one.

Example:
```elisp
(shove '(1 2 3 4 5) 0)
```
)" };

    static ALObjectPtr Fshove(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        auto element = eval->eval(obj->i(1));

        list->children().insert(std::begin(list->children()), element);

        return list;
    }
};

struct Slength
{
    inline static const std::string name = "length";

    inline static const std::string doc{ R"((length LIST)

Return the number of elements in LIST.

Example:
```elisp
(length '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Flength(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto l = eval->eval(obj->i(0));
        AL_CHECK(assert_list(l));

        return make_int(std::size(*l));
    }
};

struct Snth
{
    inline static const std::string name = "nth";

    inline static const std::string doc{ R"((nth LIST INDEX)

Return the element of LIST that is at position INDEX.

Example:
```elisp
(nth '(1 2 3 4 5) 1)
```
)" };

    static ALObjectPtr Fnth(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval->eval(obj->i(0));

        auto index = eval_check(eval, obj, 1, &assert_int<size_t>);


        if (pstring(list))
        {
            return make_char(list->to_string()[static_cast<size_t>(index->to_int())]);
        }

        AL_CHECK(assert_list(list));

        if (static_cast<ALObject::list_type::size_type>(index->to_int()) >= std::size(list->children()))
        {
            throw std::runtime_error("Index out of bound!");
        }

        return list->i(static_cast<ALObject::list_type::size_type>(index->to_int()));
    }
};

struct Sfind
{
    inline static const std::string name = "find";

    inline static const std::string doc{ R"((find LIST))" };

    static ALObjectPtr Ffind(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        auto element = eval->eval(obj->i(1));

        auto ch = list->children();
        auto it = std::find_if(ch.begin(), ch.end(), [&element](auto &el) { return equal(element, el); });
        if (it == ch.end())
        {
            return Qnil;
        }

        return make_int(std::distance(ch.begin(), it));
    }
};

struct Sinsert
{
    inline static const std::string name = "insert";

    inline static const std::string doc{ R"((insert LIST INDEX ELEMENT))" };

    static ALObjectPtr Finsert(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<3>(obj));
        auto list  = eval_check(eval, obj, 0, &assert_list<size_t>);
        auto index = eval_check(eval, obj, 1, &assert_int<size_t>);

        auto el = eval->eval(obj->i(2));

        auto &ch = list->children();
        ch.insert(std::begin(ch) + index->to_int(), el);

        return list;
    }
};

struct Scontains
{
    inline static const std::string name = "contains";

    inline static const std::string doc{ R"((contains LIST ELEMENT)

Check if LIST contains ELEMENT (according to `equal`). If yes, return
`t`, and `nil` otherwise.

Example:
```elisp
(contains '(1 2 3 4 5) 3)
```
)" };

    static ALObjectPtr Fcontains(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        auto element = eval->eval(obj->i(1));

        for (auto &el : *list)
        {
            if (equal(el, element))
            {
                return Qt;
            }
        }


        return Qnil;
    }
};

struct Sclear
{
    inline static const std::string name = "clear";

    inline static const std::string doc{ R"((clear LIST)

Remove all elements from `LIST`.

Example:
```elisp
(clear '(1 2 3 4 5))
```
)" };

    static ALObjectPtr Fclear(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        list->children().clear();
        return list;
    }
};

struct Slist
{
    inline static const std::string name = "list";

    inline static const std::string doc{ R"((list [[ELEMENT] ...])

Creates a list with arbitrary  number of elements.

Example:
```elisp
(list 1 2 3)
```
)" };


    static ALObjectPtr Flist(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_min_size<0>(obj));
        ALObject::list_type new_list{};
        for (auto el : *obj)
        {
            new_list.push_back(eval->eval(el));
        }
        return make_list(new_list);
    }
};

struct Sdelete
{
    inline static const std::string name = "delete";

    inline static const std::string doc{ R"((delete LIST ELEMENT)

Remove an element from LIST that is equal (with `equal`) to
ELEMENT. This function operates inplace, so list is changed and no new
list is created. 
Example:
```elisp
(delete '(1 2 3 4 5) 5)
```
)" };

    static ALObjectPtr Fdelete(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        auto element = eval->eval(obj->i(1));

        auto &children = list->children();

        children.erase(std::remove_if(
          std::begin(children), std::end(children), [element](ALObjectPtr t_obj) { return equal(element, t_obj); }));

        return list;
    }
};

struct Sremove
{
    inline static const std::string name = "remove";

    inline static const std::string doc{ R"((remove LIST ELEMENT)

Remove an element from LIST that is equal (with `equal`) to
ELEMENT. This function __does not__ operate inplace, so a new list is
created.

Example:
```elisp
(delete '(1 2 3 4 5) 5)
```

)" };

    static ALObjectPtr Fremove(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));
        auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

        auto element   = eval->eval(obj->i(1));
        auto &children = list->children();
        ALObject::list_type new_children;
        new_children.reserve(std::size(children));

        std::copy_if(std::begin(children),
                     std::end(children),
                     std::back_inserter(new_children),
                     [element](ALObjectPtr t_obj) { return !equal(element, t_obj); });

        return make_object(new_children);
    }
};


ALObjectPtr Fdelq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

    auto element = eval->eval(obj->i(1));

    auto &children = list->children();

    children.erase(std::remove_if(
      std::begin(children), std::end(children), [element](ALObjectPtr t_obj) { return eq(element, t_obj); }));

    return list;
}

ALObjectPtr Fremq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval_check(eval, obj, 0, &assert_list<size_t>);

    auto element   = eval->eval(obj->i(1));
    auto &children = list->children();
    ALObject::list_type new_children;
    new_children.reserve(std::size(children));

    std::copy_if(std::begin(children),
                 std::end(children),
                 std::back_inserter(new_children),
                 [element](ALObjectPtr t_obj) { return !eq(element, t_obj); });

    return make_object(new_children);
}

struct Srange
{
    inline static const std::string name = "range";

    inline static const std::string doc{ R"(((range FROM TO STEP)

Generate the range of numbers [FROM, TO) with a step STEP. All of
the arguments must be ints.

Example:
```elisp
(range 0 100 2)
```
))" };

    static ALObjectPtr Frange(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<2>(obj));

        auto start = eval_check(eval, obj, 0, &assert_int<size_t>);

        auto end = eval_check(eval, obj, 1, &assert_int<size_t>);


        const auto step = [&obj, &eval]() {
            if (std::size(*obj) > 2)
            {
                auto step_obj = eval->eval(obj->i(2));
                AL_CHECK(assert_int(step_obj));
                return step_obj->to_int();
            }
            return static_cast<ALObject::int_type>(1);
        }();

        if (step == 0)
        {
            return Qnil;
        }

        ALObject::list_type nums;
        for (auto i = start->to_int(); i < end->to_int(); i += step)
        {
            nums.push_back(make_int(i));
        }

        return make_object(nums);
    }
};


}  // namespace alisp
