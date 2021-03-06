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


struct mmap
{
    static inline const std::string name{ "buffer-mmap" };

    static inline const std::string doc{ R"((buffer-mmap BUFFER-SOURCE BUFFER-DEST SIZE)

Copy `SIZE` bytes of `BUFFER-SOURCE` to `BUFFER-DEST`.
)" };

    inline static const Signature signature{ Memory{}, Memory{}, Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem_source = arg_eval(eval, obj, 0);
        auto mem_target = arg_eval(eval, obj, 1);
        auto size       = arg_eval(eval, obj, 2);


        auto &buf_s = MemoryHelpers::get_buffer(mem_source);
        auto &buf_t = MemoryHelpers::get_buffer(mem_target);
        for (size_t i = 0; i < static_cast<size_t>(size->to_int()); ++i)
        {
            *(buf_t.m_ptr + i) = *(buf_s.m_ptr + i);
        }


        return Qt;
    }
};

struct get_size
{
    static inline const std::string name{ "buffer-size" };

    static inline const std::string doc{ R"((buffer-get-size BUFFER)

Return the size of the given buffer.
)" };

    inline static const Signature signature{ Memory{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem = arg_eval(eval, obj, 0);

        return make_int(MemoryHelpers::get_buffer(mem).m_size);
    }
};

struct set_nth_byte
{
    static inline const std::string name{ "buffer-nth-set" };

    static inline const std::string doc{ R"((buffer-nth-set BUFFER INDEX VALUE)

Set the value of the `BUFFER` at the given index to `VALUE`.
)" };

    inline static const Signature signature{ Memory{}, Int{}, Byte{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem  = arg_eval(eval, obj, 0);
        auto i    = arg_eval(eval, obj, 1);
        auto byte = arg_eval(eval, obj, 2);

        *(MemoryHelpers::get_buffer(mem).m_ptr + i->to_int()) = static_cast<unsigned char>(byte->to_int());
        return Qt;
    }
};

struct get_nth_byte
{
    static inline const std::string name{ "buffer-nth-get" };

    static inline const std::string doc{ R"((buffer-nth-get BUFFER INDEX)

Return the value of the `BUFFER` at the given index.
)" };

    inline static const Signature signature{ Memory{}, Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem = arg_eval(eval, obj, 0);
        auto i   = arg_eval(eval, obj, 1);

        return make_int(static_cast<ALObject::int_type>(*(MemoryHelpers::get_buffer(mem).m_ptr + i->to_int())));
    }
};

struct get_range
{
    static inline const std::string name{ "buffer-range-get" };

    static inline const std::string doc{ R"(((buffer-range-get BUFFER START INDEX)

Return part of a buffer as byte array. The returned bytes are in the
range [`START`, `INDEX`)
))" };

    inline static const Signature signature{ Memory{}, Int{}, Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem   = arg_eval(eval, obj, 0);
        auto start = arg_eval(eval, obj, 1);
        auto end   = arg_eval(eval, obj, 2);

        ALObject::list_type bytes;
        auto &buf = MemoryHelpers::get_buffer(mem);
        for (size_t i = static_cast<size_t>(start->to_int()); i < static_cast<size_t>(end->to_int()); ++i)
        {
            bytes.push_back(make_int(static_cast<ALObject::int_type>(*(buf.m_ptr + i))));
        }

        return make_object(bytes);
    }
};

struct fill_bytes
{
    static inline const std::string name{ "buffer-fill" };

    static inline const std::string doc{ R"((buffer-fill BUFFER VALUE)

Fill the entirety of a buffer with `VALUE`.
)" };

    inline static const Signature signature{ Memory{}, Byte{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem = arg_eval(eval, obj, 0);
        auto val = arg_eval(eval, obj, 1);

        auto &buf = MemoryHelpers::get_buffer(mem);
        for (size_t i = 0; i < buf.m_size; ++i)
        {
            *(buf.m_ptr + i) = static_cast<unsigned char>(val->to_int());
        }

        return Qt;
    }
};

struct set_bytes
{
    static inline const std::string name{ "buffer-set" };

    static inline const std::string doc{ R"((buffer-set BUFFER BYTE-ARRAY)

Set the contents of a `BUFFER` to the values in the given byte array.
)" };

    inline static const Signature signature{ Memory{}, ByteArray{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem   = arg_eval(eval, obj, 0);
        auto array = arg_eval(eval, obj, 1);

        auto &buf = MemoryHelpers::get_buffer(mem);
        for (size_t i = 0; i < array->size(); ++i)
        {
            *(buf.m_ptr + i) = static_cast<unsigned char>(array->i(i)->to_int());
        }

        return Qt;
    }
};

struct get_bytes
{
    static inline const std::string name{ "buffer-get" };

    static inline const std::string doc{ R"((buffer-get BUFFER)

Return the contents of a buffer as a byte array.
)" };

    inline static const Signature signature{ Memory{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem = arg_eval(eval, obj, 0);

        ALObject::list_type bytes;
        auto &buf = MemoryHelpers::get_buffer(mem);
        for (size_t i = 0; i < buf.m_size; ++i)
        {
            bytes.push_back(make_int(static_cast<ALObject::int_type>(*(buf.m_ptr + i))));
        }

        return make_object(bytes);
    }
};

struct allocate_buffer
{
    static inline const std::string name{ "buffer-allocate" };

    static inline const std::string doc{ R"((buffer-allocate SIZE)

Allocate a buffer of size `SIZE` and return a resource object for the
newly created buffer. The buffer can then be used with other function
for reading and writing bytes to it.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto size = arg_eval(eval, obj, 0);

        return MemoryHelpers::allocate_buffer(static_cast<size_t>(size->to_int()));
    }
};

struct release_buffer
{
    static inline const std::string name{ "buffer-release" };

    static inline const std::string doc{ R"((buffer-release BUFFER)

Deallocate `BUFFER` (resource object) and free the used memory.
)" };

    inline static const Signature signature{ Memory{} };


    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto mem = arg_eval(eval, obj, 0);

        MemoryHelpers::release_buffer(mem);
        return Qt;
    }
};


struct module_doc
{
    inline static const std::string doc{ R"(The `memory` modules provides utilities for working with raw memory
buffers. Memory buffers are just places in memory that are filled with bytes.
)" };
};

}  // namespace detail

env::ModulePtr init_memory(env::Environment *, eval::Evaluator *)
{

    auto Mmemory = module_init("memory");
    auto mem_ptr = Mmemory.get();

    module_doc(mem_ptr, detail::module_doc::doc);

    using namespace detail;

    module_defun(mem_ptr, mmap::name, mmap::func, mmap::doc, mmap::signature.al());
    module_defun(mem_ptr, get_size::name, get_size::func, get_size::doc, get_size::signature.al());
    module_defun(mem_ptr, set_nth_byte::name, set_nth_byte::func, set_nth_byte::doc, set_nth_byte::signature.al());
    module_defun(mem_ptr, get_nth_byte::name, get_nth_byte::func, get_nth_byte::doc, get_nth_byte::signature.al());
    module_defun(mem_ptr, get_range::name, get_range::func, get_range::doc, get_range::signature.al());
    module_defun(mem_ptr, fill_bytes::name, fill_bytes::func, fill_bytes::doc, fill_bytes::signature.al());
    module_defun(mem_ptr, set_bytes::name, set_bytes::func, set_bytes::doc, set_bytes::signature.al());
    module_defun(mem_ptr, get_bytes::name, get_bytes::func, get_bytes::doc, get_bytes::signature.al());
    module_defun(
      mem_ptr, allocate_buffer::name, allocate_buffer::func, allocate_buffer::doc, allocate_buffer::signature.al());
    module_defun(
      mem_ptr, release_buffer::name, release_buffer::func, release_buffer::doc, release_buffer::signature.al());


    return Mmemory;
}


}  // namespace alisp
