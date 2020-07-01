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
    static inline const std::string name{"buffer-mmap"};

    static inline const std::string doc{R"((buffer-mmap BUFFER-SOURCE BUFFER-DEST SIZE)

Copy `SIZE` bytes of `BUFFER-SOURCE` to `BUFFER-DEST`.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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
        for (size_t i = 0; i < static_cast<size_t>(size->to_int()); ++i)
        {
            *(buf_t.m_ptr + i) = *(buf_s.m_ptr + i);
        }


        return Qt;
    }

};

struct get_size
{
    static inline const std::string name{"buffer-size"};

    static inline const std::string doc{R"((buffer-get-size BUFFER)

Return the size of the given buffer.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto mem = eval->eval(t_obj->i(0));
        AL_CHECK(assert_memory(mem));

        return make_int(MemoryHelpers::get_buffer(mem).m_size);
    }

};

struct set_nth_byte
{
    static inline const std::string name{"buffer-nth-set"};

    static inline const std::string doc{R"((buffer-nth-set BUFFER INDEX VALUE)

Set the value of the `BUFFER` at the given index to `VALUE`.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

};

struct get_nth_byte
{
    static inline const std::string name{"buffer-nth-get"};

    static inline const std::string doc{R"((buffer-nth-get BUFFER INDEX)

Return the value of the `BUFFER` at the given index.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));
        auto mem = eval->eval(t_obj->i(0));
        auto i   = eval->eval(t_obj->i(1));
        AL_CHECK(assert_memory(mem));
        AL_CHECK(assert_int(i));

        return make_int(static_cast<ALObject::int_type>(*(MemoryHelpers::get_buffer(mem).m_ptr + i->to_int())));
    }

};

struct get_range
{
    static inline const std::string name{"buffer-range-get"};

    static inline const std::string doc{R"(((buffer-range-get BUFFER START INDEX)

Return part of a buffer as byte array. The returned bytes are in the
range [`START`, `INDEX`)
))"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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
        {
            bytes.push_back(make_int(static_cast<ALObject::int_type>(*(buf.m_ptr + i))));
        }

        return make_object(bytes);
    }

};

struct fill_bytes
{
    static inline const std::string name{"buffer-fill"};

    static inline const std::string doc{R"((buffer-fill BUFFER VALUE)

Fill the entirety of a buffer with `VALUE`.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));
        auto mem = eval->eval(t_obj->i(0));
        auto val = eval->eval(t_obj->i(1));
        AL_CHECK(assert_memory(mem));
        AL_CHECK(assert_byte(val));

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
    static inline const std::string name{"buffer-set"};

    static inline const std::string doc{R"((buffer-set BUFFER BYTE-ARRAY)

Set the contents of a `BUFFER` to the values in the given byte array.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));
        auto mem   = eval->eval(t_obj->i(0));
        auto array = eval->eval(t_obj->i(1));
        AL_CHECK(assert_memory(mem));
        AL_CHECK(assert_byte_array(array));

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
    static inline const std::string name{"buffer-get"};

    static inline const std::string doc{R"((buffer-get BUFFER)

Return the contents of a buffer as a byte array.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto mem = eval->eval(t_obj->i(0));
        AL_CHECK(assert_memory(mem));

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
    static inline const std::string name{"buffer-allocate"};

    static inline const std::string doc{R"((buffer-allocate SIZE)

Allocate a buffer of size `SIZE` and return a resource object for the
newly created buffer. The buffer can then be used with other function
for reading and writing bytes to it.
)"};

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto size = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(size));

        return MemoryHelpers::allocate_buffer(static_cast<size_t>(size->to_int()));
    }

};

struct release_buffer
{
    static inline const std::string name{"buffer-release"};

    static inline const std::string doc{R"((buffer-release BUFFER)

Deallocate `BUFFER` (resource object) and free the used memory.
)"};
    

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto mem = eval->eval(t_obj->i(0));
        AL_CHECK(assert_memory(mem));

        MemoryHelpers::release_buffer(mem);
        return Qt;
    }

};



struct module_doc
{
    inline static const std::string doc{R"(The `memory` modules provides utilities for working with raw memory
buffers. Memory buffers are just places in memory that are filled with bytes.
)"};
    
};

}  // namespace detail

env::ModulePtr init_memory(env::Environment *, eval::Evaluator *)
{

    auto Mmemory = module_init("memory");
    auto mem_ptr = Mmemory.get();

    module_doc(mem_ptr, detail::module_doc::doc);


    return Mmemory;
}


}  // namespace alisp
