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


struct Spsym
{
    inline static const std::string name = "psym";

    inline static const std::string doc{ R"((psym FORM)

Return `t` if FORM is a symbol and `nil` otherwise.
)" };

    static ALObjectPtr Fpsym(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return psym(evl->eval(obj->i(0))) ? Qt : Qnil;
    }
};

struct Splist
{
    inline static const std::string name = "plist";

    inline static const std::string doc{ R"((plist FORM)

Return `t` if FORM is a list and `nil` otherwise.
)" };

    static ALObjectPtr Fplist(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return plist(evl->eval(obj->i(0))) ? Qt : Qnil;
    }
};

struct Spint
{
    inline static const std::string name = "pint";

    inline static const std::string doc{ R"((pint FORM)

Return `t` if FORM is a integer value and `nil` otherwise.
)" };

    static ALObjectPtr Fpint(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return pint(evl->eval(obj->i(0))) ? Qt : Qnil;
    }
};

struct Spreal
{
    inline static const std::string name = "preal";

    inline static const std::string doc{ R"((preal FORM)

Return `t` if FORM is a real value and `nil` otherwise.
)" };

    static ALObjectPtr Fpreal(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return preal(evl->eval(obj->i(0))) ? Qt : Qnil;
    }
};

struct Spstring
{
    inline static const std::string name = "pstring";

    inline static const std::string doc{ R"((pstring FORM)

Return `t` if FORM is a string and `nil` otherwise.
)" };

    static ALObjectPtr Fpstring(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return pstring(evl->eval(obj->i(0))) ? Qt : Qnil;
    }
};

struct Spfunction
{
    inline static const std::string name = "pfunction";

    inline static const std::string doc{ R"((pfunction FORM)

Return `t` if FORM is a function and `nil` otherwise.
)" };

    static ALObjectPtr Fpfunction(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return pfunction(evl->eval(obj->i(0))) ? Qt : Qnil;
    }
};

struct Spfile
{
    inline static const std::string name = "pfile";

    inline static const std::string doc{ R"((pfile FORM)

Return `t` if FORM is a string and `nil` otherwise.
)" };

    static ALObjectPtr Fpfile(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto id = eval_check(eval, obj, 0, &assert_int<size_t>);
        return files::files_registry.belong(object_to_resource(id)) ? Qt : Qnil;
    }
};

struct Spstream
{
    inline static const std::string name = "pstream";

    inline static const std::string doc{ R"((pstream FORM)

Return `t` if FORM is a string and `nil` otherwise.
)" };

    static ALObjectPtr Fpstream(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto id = eval_check(eval, obj, 0, &assert_int<size_t>);
        return al::streams_registry.belong(object_to_resource(id)) ? Qt : Qnil;
    }
};

struct Spmemory
{
    inline static const std::string name = "pmemory";

    inline static const std::string doc{ R"((pmemory FORM)

Return `t` if FORM is a string and `nil` otherwise.
)" };

    static ALObjectPtr Fpmemory(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto id = eval_check(eval, obj, 0, &assert_int<size_t>);
        return memory::memory_registry.belong(object_to_resource(id)) ? Qt : Qnil;
    }
};

struct Spbyte
{
    inline static const std::string name = "pbyte";

    inline static const std::string doc{ R"((pbyte FORM)

Return `t` if FORM is a string and `nil` otherwise.
)" };

    static ALObjectPtr Fpbyte(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto ob = eval->eval(obj->i(0));
        if (!pint(ob))
        {
            return Qnil;
        }
        const auto val = ob->to_int();
        return (0 <= val and val <= 255) ? Qt : Qnil;
    }
};


}  // namespace alisp
