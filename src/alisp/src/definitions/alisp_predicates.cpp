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

#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/alisp/declarations/predicates.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


ALObjectPtr Fpsym(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return psym(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fplist(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return plist(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpint(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return pint(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpreal(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return preal(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpstring(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return pstring(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpfunction(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return pfunction(evl->eval(obj->i(0))) ? Qt : Qnil;
}

ALObjectPtr Fpfile(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return files::files_registry.belong(object_to_resource(evl->eval(obj->i(0)))) ? Qt : Qnil;
}

ALObjectPtr Fpstream(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return al::streams_registry.belong(object_to_resource(evl->eval(obj->i(0)))) ? Qt : Qnil;
}

ALObjectPtr Fpmemory(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return memory::memory_registry.belong(object_to_resource(evl->eval(obj->i(0)))) ? Qt : Qnil;
}

ALObjectPtr Fpbyte(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    auto ob = evl->eval(obj->i(0));
    AL_CHECK(assert_int(ob));
    if (!pint(ob))
    {
        return Qnil;
    }
    const auto val = ob->to_int();
    return (0 <= val and val <= 255) ? Qt : Qnil;
}


}  // namespace alisp
