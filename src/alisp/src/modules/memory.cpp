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


#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_memory.hpp"

#include "alisp/utility/env.hpp"

namespace alisp
{


// extern ALObjectPtr  MemoryHelpers::allocate_buffer(size_t t_size);
// extern memory::MemoryBuffer & MemoryHelpers::get_buffer(ALObjectPtr
// t_buffer); extern void  MemoryHelpers::release_buffer(ALObjectPtr t_buffer);

namespace detail
{


ALObjectPtr Fmmap(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(t_obj));
    auto mem_source = eval->eval(t_obj->i(0));
    auto mem_target = eval->eval(t_obj->i(1));
    auto size       = eval->eval(t_obj->i(2));

    AL_CHECK(assert_memory(mem_source));
    AL_CHECK(assert_memory(mem_target));
    AL_CHECK(assert_int(size));


    auto &buf_s = MemoryHelpers::get_buffer(mem_source);
    auto &buf_t = MemoryHelpers::get_buffer(mem_target);
    for (size_t i = 0; i < static_cast<size_t>(size->to_int()); ++i) { *(buf_t.m_ptr + i) = *(buf_s.m_ptr + i); }


    return Qt;
}

ALObjectPtr Fget_size(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto mem = eval->eval(t_obj->i(0));
    AL_CHECK(assert_memory(mem));

    return make_int(MemoryHelpers::get_buffer(mem).m_size);
}

ALObjectPtr Fset_nth_byte(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(t_obj));
    auto mem  = eval->eval(t_obj->i(0));
    auto i    = eval->eval(t_obj->i(1));
    auto byte = eval->eval(t_obj->i(2));
    AL_CHECK(assert_memory(mem));
    AL_CHECK(assert_int(i));
    AL_CHECK(assert_byte(byte));

    *(MemoryHelpers::get_buffer(mem).m_ptr + i->to_int()) = static_cast<unsigned char>(byte->to_int());
    return Qt;
}

ALObjectPtr Fget_nth_byte(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(t_obj));
    auto mem = eval->eval(t_obj->i(0));
    auto i   = eval->eval(t_obj->i(1));
    AL_CHECK(assert_memory(mem));
    AL_CHECK(assert_int(i));

    return make_int(static_cast<ALObject::int_type>(*(MemoryHelpers::get_buffer(mem).m_ptr + i->to_int())));
}

ALObjectPtr Fget_range(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<3>(t_obj));
    auto mem   = eval->eval(t_obj->i(0));
    auto start = eval->eval(t_obj->i(1));
    auto end   = eval->eval(t_obj->i(2));
    AL_CHECK(assert_memory(mem));
    AL_CHECK(assert_int(start));
    AL_CHECK(assert_int(end));

    ALObject::list_type bytes;
    auto &buf = MemoryHelpers::get_buffer(mem);
    for (size_t i = static_cast<size_t>(start->to_int()); i < static_cast<size_t>(end->to_int()); ++i)
    { bytes.push_back(make_int(static_cast<ALObject::int_type>(*(buf.m_ptr + i)))); }

    return make_object(bytes);
}

ALObjectPtr Ffill_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(t_obj));
    auto mem = eval->eval(t_obj->i(0));
    auto val = eval->eval(t_obj->i(1));
    AL_CHECK(assert_memory(mem));
    AL_CHECK(assert_byte(val));

    auto &buf = MemoryHelpers::get_buffer(mem);
    for (size_t i = 0; i < buf.m_size; ++i) { *(buf.m_ptr + i) = static_cast<unsigned char>(val->to_int()); }

    return Qt;
}

ALObjectPtr Fset_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<2>(t_obj));
    auto mem   = eval->eval(t_obj->i(0));
    auto array = eval->eval(t_obj->i(1));
    AL_CHECK(assert_memory(mem));
    AL_CHECK(assert_byte_array(array));

    auto &buf = MemoryHelpers::get_buffer(mem);
    for (size_t i = 0; i < array->size(); ++i) { *(buf.m_ptr + i) = static_cast<unsigned char>(array->i(i)->to_int()); }

    return Qt;
}

ALObjectPtr Fget_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto mem = eval->eval(t_obj->i(0));
    AL_CHECK(assert_memory(mem));

    ALObject::list_type bytes;
    auto &buf = MemoryHelpers::get_buffer(mem);
    for (size_t i = 0; i < buf.m_size; ++i)
    { bytes.push_back(make_int(static_cast<ALObject::int_type>(*(buf.m_ptr + i)))); }

    return make_object(bytes);
}

ALObjectPtr Fallocate_buffer(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto size = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(size));

    return MemoryHelpers::allocate_buffer(static_cast<size_t>(size->to_int()));
}

ALObjectPtr Frelease_buffer(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto mem = eval->eval(t_obj->i(0));
    AL_CHECK(assert_memory(mem));

    MemoryHelpers::release_buffer(mem);
    return Qt;
}


}  // namespace detail

env::ModulePtr init_memory(env::Environment *, eval::Evaluator *)
{

    auto Mmemory = module_init("memory");
    auto mem_ptr = Mmemory.get();

    module_doc(mem_ptr,
               R"(The `memory` modules provides utilities for working with raw memory
buffers. Memory buffers are just places in memory that are filled with bytes.)");

    module_defun(mem_ptr, "buffer-allocate", &detail::Fallocate_buffer);
    module_defun(mem_ptr, "buffer-release", &detail::Frelease_buffer);
    module_defun(mem_ptr, "buffer-mmap", &detail::Fmmap);
    module_defun(mem_ptr, "buffer-get-size", &detail::Fget_size);
    module_defun(mem_ptr, "buffer-nth-get", &detail::Fget_nth_byte);
    module_defun(mem_ptr, "buffer-nth-set", &detail::Fset_nth_byte);
    module_defun(mem_ptr, "buffer-range-get", &detail::Fget_range);
    module_defun(mem_ptr, "buffer-fill", &detail::Ffill_bytes);
    module_defun(mem_ptr, "buffer-set", &detail::Fset_bytes);
    module_defun(mem_ptr, "buffer-get", &detail::Fget_bytes);

    return Mmemory;
}


}  // namespace alisp
