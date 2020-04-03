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
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


ALObjectPtr Fmapc(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
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
                eval->handle_lambda(fun_obj, make_list(quote(el)));
            }
            else
            {
                eval->handle_lambda(fun_obj, make_list(el));
            }
        }
        return Qt;
    }
    else if (pstring(list))
    {

        for (auto &el : list->to_string())
        {
            eval->handle_lambda(fun_obj, make_list(make_char(el)));
        }
        return Qt;
    }


    return Qnil;
}

ALObjectPtr Fmapcar(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
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
            new_l.push_back(eval->handle_lambda(fun_obj, make_list(quote(el))));
        }
        else
        {
            new_l.push_back(eval->handle_lambda(fun_obj, make_list(el)));
        }
    }

    return make_list(new_l);
}

ALObjectPtr Fcar(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto list = eval->eval(obj->i(0));
    return list->i(0);
}

ALObjectPtr Fcons(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    return splice(list, 1);
}

ALObjectPtr Fhead(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    return Fcar(obj, env, eval);
}

ALObjectPtr Ftail(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    return Fcons(obj, env, eval);
}

ALObjectPtr Flast(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    return list->i(list->length() - 1);
}

ALObjectPtr Finit(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    return splice(list, 0, static_cast<ALObject::list_type::difference_type>(list->length() - 1));
}

ALObjectPtr Fpush(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    auto element = eval->eval(obj->i(1));

    list->children().push_back(element);

    return list;
}

ALObjectPtr Fshove(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    auto element = eval->eval(obj->i(1));

    list->children().insert(std::begin(list->children()), element);

    return list;
}

ALObjectPtr Flength(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto l = eval->eval(obj->i(0));
    AL_CHECK(assert_list(l));

    return make_int(std::size(*l));
}

ALObjectPtr Fnth(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list  = eval->eval(obj->i(0));
    auto index = eval->eval(obj->i(1));
    AL_CHECK(assert_int(index));

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

ALObjectPtr Ffind(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list    = eval->eval(obj->i(0));
    auto element = eval->eval(obj->i(1));
    AL_CHECK(assert_list(list));

    auto ch = list->children();
    auto it = std::find_if(ch.begin(), ch.end(), [&element](auto &el) { return equal(element, el); });
    if (it == ch.end())
    {
        return Qnil;
    }

    return make_int(std::distance(ch.begin(), it));
}

// inplace
ALObjectPtr Finsert(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(obj));
    auto list  = eval->eval(obj->i(0));
    auto index = eval->eval(obj->i(1));
    auto el    = eval->eval(obj->i(2));
    AL_CHECK(assert_int(index));
    AL_CHECK(assert_list(list));

    auto &ch = list->children();
    ch.insert(std::begin(ch) + index->to_int(), el);

    return list;
}

ALObjectPtr Fcontains(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
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

ALObjectPtr Fclear(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    list->children().clear();
    return list;
}

ALObjectPtr Flist(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<0>(obj));
    ALObject::list_type new_list{};
    for (auto el : *obj)
    {
        new_list.push_back(eval->eval(el));
    }
    return make_list(new_list);
}

// inplace
ALObjectPtr Fdelete(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    auto element = eval->eval(obj->i(1));

    auto &children = list->children();

    children.erase(std::remove_if(
      std::begin(children), std::end(children), [element](ALObjectPtr t_obj) { return equal(element, t_obj); }));

    return list;
}

// return copy
ALObjectPtr Fremove(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
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

// inplace
ALObjectPtr Fdelq(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
    auto element = eval->eval(obj->i(1));

    auto &children = list->children();

    children.erase(std::remove_if(
      std::begin(children), std::end(children), [element](ALObjectPtr t_obj) { return eq(element, t_obj); }));

    return list;
}

// return copy
ALObjectPtr Fremq(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(obj));
    auto list = eval->eval(obj->i(0));
    AL_CHECK(assert_list(list));
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

ALObjectPtr Frange(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));

    auto start = eval->eval(obj->i(0));
    auto end   = eval->eval(obj->i(1));

    AL_CHECK(assert_int(start));
    AL_CHECK(assert_int(end));

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


}  // namespace alisp
