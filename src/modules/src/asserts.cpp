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

ALObjectPtr Fassert_numbers(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    
    if(auto val = eval->eval(t_obj->i(0)); are_objects_numbers(val))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Fassert_symbol(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    
    if(auto val = eval->eval(t_obj->i(0)); !psym(val))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Fassert_string(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    if(auto val = eval->eval(t_obj->i(0)); !pstring(val))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Fassert_list(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));

    if(auto val = eval->eval(t_obj->i(0)); !plist(val))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Fassert_number(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    if(auto val = eval->eval(t_obj->i(0)); !val->is_int() and !val->is_real())
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Fassert_int(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));

    if(auto val = eval->eval(t_obj->i(0)); !pint(val))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
    
}

ALObjectPtr Fassert_char(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    
    if(auto val = eval->eval(t_obj->i(0)); !val->is_int() and !val->check_char_flag())
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
    
}

ALObjectPtr Fassert_function(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));

    if(auto val = eval->eval(t_obj->i(0)); !val->check_function_flag())
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;
    
}

ALObjectPtr Fassert_non_const(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));

    if(auto val = eval->eval(t_obj->i(0)); !val->check_const_flag())
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;    

}

ALObjectPtr Fassert_file(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    
    AL_CHECK(assert_size<1>(t_obj));

    if(auto val = eval->eval(t_obj->i(0)); !files::files_registry.belong(object_to_resource(val)))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;    

}

ALObjectPtr Fassert_stream(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    
    if(auto val = eval->eval(t_obj->i(0)); !al::streams_registry.belong(object_to_resource(val)))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;    

}

ALObjectPtr Fassert_byte(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    if (is_falsy(Vdebug_mode))
    {
        return Qt;
    }
    AL_CHECK(assert_size<1>(t_obj));
    
    if(auto val = eval->eval(t_obj->i(0)); !pint(val) || !(0 <= val->to_int() and val->to_int() <= 255))
    {
        throw signal_exception(
            env::intern("assert-signal"),
            make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
    }
    return Qt;    
}

ALObjectPtr Fassert_byte_array(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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
            throw signal_exception(
                env::intern("assert-signal"),
                make_object(make_string("Assertion failed."), make_string(dump(t_obj->i(0))), make_string(dump(val))));
        }
    }

    return Qt;
    
}



}

ALISP_EXPORT alisp::env::ModulePtr init_asserts(alisp::env::Environment*, alisp::eval::Evaluator*)
{
    using namespace alisp;
    auto assert_module     = alisp::module_init("asserts");
    auto ass_ptr = assert_module.get();

    alisp::module_defun(ass_ptr, "assert-numbers", &asserts::Fassert_numbers, R"()");
    alisp::module_defun(ass_ptr, "assert-symbol", &asserts::Fassert_symbol, R"()");
    alisp::module_defun(ass_ptr, "assert-string", &asserts::Fassert_string, R"()");
    alisp::module_defun(ass_ptr, "assert-list", &asserts::Fassert_list, R"()");
    alisp::module_defun(ass_ptr, "assert-number", &asserts::Fassert_number, R"()");
    alisp::module_defun(ass_ptr, "assert-int", &asserts::Fassert_int, R"()");
    alisp::module_defun(ass_ptr, "assert-char", &asserts::Fassert_char, R"()");
    alisp::module_defun(ass_ptr, "assert-function", &asserts::Fassert_function, R"()");
    alisp::module_defun(ass_ptr, "assert-non-const", &asserts::Fassert_non_const, R"()");
    alisp::module_defun(ass_ptr, "assert-file", &asserts::Fassert_file, R"()");
    alisp::module_defun(ass_ptr, "assert-stream", &asserts::Fassert_stream, R"()");
    alisp::module_defun(ass_ptr, "assert-byte", &asserts::Fassert_byte, R"()");
    alisp::module_defun(ass_ptr, "assert-byte-array", &asserts::Fassert_byte_array, R"()");

    return assert_module;
}
