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

#include "alisp/alisp/declarations/logic.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


struct Sand
{
    inline static const std::string name = "and";

    inline static const std::string doc{ R"((and [[VALUE]...])

Return `t` if all of the arguments evaluates to a truthy
value. The arguments are lazily evaluated.
)" };

    static ALObjectPtr Fand(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        for (auto el : *obj)
        {
            if (is_falsy(evl->eval(el)))
            {
                return Qnil;
            }
        }
        return Qt;
    }
};

struct Sor
{
    inline static const std::string name = "or";

    inline static const std::string doc{ R"((or [[VALUE]...])

Return `t` if at least one of the arguments evaluates to a truthy
value. The arguments are lazily evaluated.

)" };

    static ALObjectPtr For(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        for (auto el : *obj)
        {
            if (is_truthy(evl->eval(el)))
            {
                return Qt;
            }
        }
        return Qnil;
    }
};

struct Snot
{
    inline static const std::string name = "no";

    inline static const std::string doc{ R"((not FORM)

Return `t` if FORM evaluate to a falsey value and `nil` otherwise. 
)" };

    static ALObjectPtr Fnot(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));

        bool sum = is_truthy(evl->eval(obj->i(0)));
        return !sum ? Qt : Qnil;
    }
};


}  // namespace alisp
