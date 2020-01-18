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
#include <string>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

namespace alisp
{

ALObjectPtr Fslice(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fsort(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return Qnil;
}

ALObjectPtr Fzip(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<2>(obj);

    auto eval_list = eval_transform(eval, obj);
    ALObject::list_type new_list{};
    size_t min_size = eval_list->i(0)->children().size();
    for (auto& l : *eval_list) {
        auto curr_size = std::size(l->children());
        if (curr_size < min_size) { min_size = curr_size; }
    }

    for (size_t i = 0; i < min_size; ++i) {
        ALObject::list_type next_tuple{};

        for (auto& el : *eval_list) {
            next_tuple.push_back(el->children()[i]) ;
        }
        
        new_list.push_back(make_object(next_tuple));
    }

    return make_object(new_list);
}

ALObjectPtr Ffilter(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fany(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}

ALObjectPtr Fall(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<2>(obj);
    return Qnil;
}



}
