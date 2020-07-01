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
#include "alisp/alisp/alisp_object.hpp"

namespace asserts
{

using namespace alisp;

struct assert_numbers
{

    inline static const std::string name{ "assert-numbers" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); are_objects_numbers(val))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_symbol
{
    inline static const std::string name{ "assert-symbol" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !psym(val))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_string
{
    inline static const std::string name{ "assert-string" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));
        if (auto val = eval->eval(t_obj->i(0)); !pstring(val))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_list
{
    inline static const std::string name{ "assert-list" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !plist(val))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_number
{
    inline static const std::string name{ "assert-numbers" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));
        if (auto val = eval->eval(t_obj->i(0)); !val->is_int() and !val->is_real())
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_int
{
    inline static const std::string name{ "assert-int" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !pint(val))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_real
{
    inline static const std::string name{ "assert-real" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !preal(val))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_char
{
    inline static const std::string name{ "assert-char" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !val->is_int() and !val->check_char_flag())
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_function
{
    inline static const std::string name{ "assert-function" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !val->check_function_flag())
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_non_const
{
    inline static const std::string name{ "assert-non-const" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !val->check_const_flag())
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_file
{
    inline static const std::string name{ "assert-file" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }

        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !files::files_registry.belong(object_to_resource(val)))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_stream
{
    inline static const std::string name{ "assert-stream" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !al::streams_registry.belong(object_to_resource(val)))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_byte
{
    inline static const std::string name{ "assert-byte" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));

        if (auto val = eval->eval(t_obj->i(0)); !pint(val) || !(0 <= val->to_int() and val->to_int() <= 255))
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
        return Qt;
    }
};

struct assert_byte_array
{
    inline static const std::string name{ "assert-byte-array" };

    inline static const Signature signature{ Any{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
        AL_CHECK(assert_size<1>(t_obj));
        auto val = eval->eval(t_obj->i(0));

        if (!val->is_list())
        {
            throw signal_exception(
              env::intern("assert-signal"),
              make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }

        for (auto &el : *val)
        {
            auto byte = el->to_int();
            if (!(0 <= byte and byte <= 255))
            {
                throw signal_exception(env::intern("assert-signal"),
                                       make_object(make_string("Assertion failed."),
                                                   make_string(dump(t_obj->i(0))),
                                                   make_string(dump(val))));
            }
        }

        return Qt;
    }
};


struct module_doc
{

    inline static const std::string doc{ R"()" };
};


}  // namespace asserts

ALISP_EXPORT alisp::env::ModulePtr init_asserts(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;
    auto assert_module = alisp::module_init("asserts");
    auto ass_ptr       = assert_module.get();

    module_doc(ass_ptr, asserts::module_doc::doc);

    module_defun(ass_ptr,
                 asserts::assert_numbers::name,
                 asserts::assert_numbers::func,
                 asserts::assert_numbers::doc,
                 asserts::assert_numbers::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_symbol::name,
                 asserts::assert_symbol::func,
                 asserts::assert_symbol::doc,
                 asserts::assert_symbol::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_string::name,
                 asserts::assert_string::func,
                 asserts::assert_string::doc,
                 asserts::assert_string::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_list::name,
                 asserts::assert_list::func,
                 asserts::assert_list::doc,
                 asserts::assert_list::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_number::name,
                 asserts::assert_number::func,
                 asserts::assert_number::doc,
                 asserts::assert_number::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_int::name,
                 asserts::assert_int::func,
                 asserts::assert_int::doc,
                 asserts::assert_int::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_real::name,
                 asserts::assert_real::func,
                 asserts::assert_real::doc,
                 asserts::assert_real::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_char::name,
                 asserts::assert_char::func,
                 asserts::assert_char::doc,
                 asserts::assert_char::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_function::name,
                 asserts::assert_function::func,
                 asserts::assert_function::doc,
                 asserts::assert_function::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_non_const::name,
                 asserts::assert_non_const::func,
                 asserts::assert_non_const::doc,
                 asserts::assert_non_const::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_file::name,
                 asserts::assert_file::func,
                 asserts::assert_file::doc,
                 asserts::assert_file::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_stream::name,
                 asserts::assert_stream::func,
                 asserts::assert_stream::doc,
                 asserts::assert_stream::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_byte::name,
                 asserts::assert_byte::func,
                 asserts::assert_byte::doc,
                 asserts::assert_byte::signature.al());
    module_defun(ass_ptr,
                 asserts::assert_byte_array::name,
                 asserts::assert_byte_array::func,
                 asserts::assert_byte_array::doc,
                 asserts::assert_byte_array::signature.al());


    return assert_module;
}
