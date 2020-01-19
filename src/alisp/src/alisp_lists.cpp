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



ALObjectPtr Fmapc(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);

    auto fun_obj = eval->eval(obj->i(0));
    auto list = eval->eval(obj->i(1));


    for (auto& el : list->children()) {
        if (psym(el) or plist(el)) {
            eval->handle_lambda(fun_obj, make_list(quote(el)));
        } else {
            eval->handle_lambda(fun_obj, make_list(el));
        }
    }

    return Qt;
}

ALObjectPtr Fcar(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    return list->i(0);
}

ALObjectPtr Fcons(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    return splice(list, 1);
}

ALObjectPtr Fhead(ALObjectPtr obj, env::Environment* env, eval::Evaluator* eval)
{
    return Fcar(obj, env, eval);
}

ALObjectPtr Ftail(ALObjectPtr obj, env::Environment* env, eval::Evaluator* eval)
{
    return Fcons(obj, env, eval);
}

ALObjectPtr Flast(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    return list->i(list->length() - 1);
}

ALObjectPtr Finit(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    return splice(list, 0, static_cast<ALObject::list_type::difference_type>(list->length() - 1));
}

ALObjectPtr Fpush(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    auto element = eval->eval(obj->i(1));
    
    list->children().push_back(element);
    
    return list;
}

ALObjectPtr Flength(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    assert_list(obj->i(0));
    
    return make_int(std::size(obj->i(0)->children()));
}

ALObjectPtr Fnth(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    auto index = eval->eval(obj->i(1));
    assert_int(index);

    if (static_cast<ALObject::list_type::size_type>(index->to_int()) >= std::size(list->children())){
        throw std::runtime_error("Index out of bound!");
    }
    
    return list->i(static_cast<ALObject::list_type::size_type>(index->to_int()));
}

//inplace
ALObjectPtr Fdelete(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    auto element = eval->eval(obj->i(1));
    
    auto& children = list->children();

    children.erase(std::remove_if(std::begin(children), std::end(children),
                                  [element](ALObjectPtr t_obj){ return equal(element, t_obj); }));
    
    return list;
}

//return copy
ALObjectPtr Fremove(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto list = eval->eval(obj->i(0));
    assert_list(list);
    auto element = eval->eval(obj->i(1));
    auto& children = list->children();
    ALObject::list_type new_children;
    new_children.reserve(std::size(children));

    std::copy_if(std::begin(children), std::end(children), std::back_inserter(new_children),
                 [element](ALObjectPtr t_obj){ return !equal(element, t_obj); });
    
    return make_object(new_children);
}

ALObjectPtr Frange(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);

    auto start = eval->eval(obj->i(0));
    auto end = eval->eval(obj->i(1));

    assert_int(start);
    assert_int(end);

    ALObject::list_type nums;
    for (auto i = start->to_int(); i < end->to_int(); ++i) {
        nums.push_back(make_int(i));
    }
    
    return make_object(nums);
}


}
