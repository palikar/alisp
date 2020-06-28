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
#pragma once

#include <unordered_map>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/declarations/constants.hpp"
#include "alisp/alisp/alisp_assertions.hpp"


namespace alisp
{

struct Int
{


    ALObjectPtr to_al() { return Qint; }
};

struct Double
{

    ALObjectPtr to_al() { return Qdouble; }
};

struct String
{

    ALObjectPtr to_al() { return Qstring; }
};

struct List
{

    ALObjectPtr to_al() { return Qlist_arg; }
};

struct Char
{

    ALObjectPtr to_al() { return Qchar_arg; }
};

struct Sym
{

    ALObjectPtr to_al() { return Qsym_arg; }
};

struct Numbers
{

    ALObjectPtr to_al() { return Qnumber_arg; }
};

struct Function
{

    ALObjectPtr to_al() { return Qfunction_arg; }
};

struct File
{

    ALObjectPtr to_al() { return Qfile_arg; }
};

struct Stream
{

    ALObjectPtr to_al() { return Qstream_arg; }
};

struct Memory
{

    ALObjectPtr to_al() { return Qmemory_arg; }
};

struct Byte
{

    ALObjectPtr to_al() { return Qbyte_arg; }
};

struct ByteArray
{

    ALObjectPtr to_al() { return Qbytearray_arg; }
};

struct Optional
{

    ALObjectPtr to_al() { return Qoptional; }
};

struct Rest
{

    ALObjectPtr to_al() { return Qrest; }
};

template<typename... Args> struct Signature
{
    std::tuple<Args...> args;
    static constexpr size_t cnt = sizeof...(Args);

    Signature(Args... t_checks) : args(std::move(t_checks)...) {}

    ALObjectPtr arglist_object() { return do_arglist(std::make_index_sequence<cnt>()); }

  private:
    template<size_t... I> ALObjectPtr do_arglist(std::index_sequence<I...>)
    {
        auto signature = make_object(std::get<I>(args).to_al()...);
        return signature;
    }
};


inline std::unordered_map<ALObject *, std::function<void( ALObjectPtr, size_t, ALObjectPtr)>> signature_assertions = {
    { Qint.get(), &assert_int<size_t, ALObjectPtr> },
    { Qdouble.get(), &assert_number<size_t, ALObjectPtr> },
    { Qstring.get(), &assert_string<size_t, ALObjectPtr> },
    { Qint.get(), &assert_int<size_t, ALObjectPtr> },
    { Qlist_arg.get(), &assert_list<size_t, ALObjectPtr> },
    { Qsym_arg.get(), &assert_symbol<size_t, ALObjectPtr> },
    { Qchar_arg.get(), &assert_char<size_t, ALObjectPtr> },
    { Qnumber_arg.get(), &assert_number<size_t, ALObjectPtr> },
    { Qfunction_arg.get(), &assert_function<size_t, ALObjectPtr> },
    { Qfile_arg.get(), &assert_file<size_t, ALObjectPtr> },
    { Qstream_arg.get(), &assert_stream<size_t, ALObjectPtr> },
    { Qmemory_arg.get(), &assert_memory<size_t, ALObjectPtr> },
    { Qbyte_arg.get(), &assert_byte<size_t, ALObjectPtr> },
    { Qbytearray_arg.get(), &assert_byte_array<size_t, ALObjectPtr> },
};


}  // namespace alisp
