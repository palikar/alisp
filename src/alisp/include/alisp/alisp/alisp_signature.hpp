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


    ALObjectPtr to_al() const { return Qint; }
};

struct Double
{

    ALObjectPtr to_al() const { return Qdouble; }
};

struct String
{

    ALObjectPtr to_al() const { return Qstring; }
};

struct List
{

    ALObjectPtr to_al() const { return Qlist_arg; }
};

struct Char
{

    ALObjectPtr to_al() const { return Qchar_arg; }
};

struct Sym
{

    ALObjectPtr to_al() const { return Qsym_arg; }
};

struct Number
{

    ALObjectPtr to_al() const { return Qnumber_arg; }
};

struct Numbers
{

    ALObjectPtr to_al() const { return Qnumbers_arg; }
};

struct Function
{

    ALObjectPtr to_al() const { return Qfunction_arg; }
};

struct File
{

    ALObjectPtr to_al() const { return Qfile_arg; }
};

struct Stream
{

    ALObjectPtr to_al() const { return Qstream_arg; }
};

struct Memory
{

    ALObjectPtr to_al() const { return Qmemory_arg; }
};

struct Byte
{

    ALObjectPtr to_al() const { return Qbyte_arg; }
};

struct ByteArray
{

    ALObjectPtr to_al() const { return Qbytearray_arg; }
};

struct Any
{

    ALObjectPtr to_al() const { return Qany_arg; }
};

struct Optional
{

    ALObjectPtr to_al() const { return Qoptional; }
};

struct Rest
{

    ALObjectPtr to_al() const { return Qrest; }
};

template<typename... Args> struct Signature
{
    std::tuple<Args...> args;
    static constexpr size_t cnt = sizeof...(Args);

    Signature(Args... t_checks) : args(std::move(t_checks)...) {}

    ALObjectPtr arglist_object() const { return do_arglist(std::make_index_sequence<cnt>()); }

  private:
    template<size_t... I> ALObjectPtr do_arglist(std::index_sequence<I...>) const
    {
        auto signature = make_object(std::get<I>(args).to_al()...);
        return signature;
    }
};

inline void ignore(ALObjectPtr, size_t, ALObjectPtr)
{
}

inline std::unordered_map<ALObject *, std::function<void(ALObjectPtr, size_t, ALObjectPtr)>> signature_assertions = {
    { Qint.get(), &assert_int<size_t, ALObjectPtr> },
    { Qdouble.get(), &assert_number<size_t, ALObjectPtr> },
    { Qstring.get(), &assert_string<size_t, ALObjectPtr> },
    { Qint.get(), &assert_int<size_t, ALObjectPtr> },
    { Qlist_arg.get(), &assert_list<size_t, ALObjectPtr> },
    { Qsym_arg.get(), &assert_symbol<size_t, ALObjectPtr> },
    { Qchar_arg.get(), &assert_char<size_t, ALObjectPtr> },
    { Qnumber_arg.get(), &assert_number<size_t, ALObjectPtr> },
    { Qnumbers_arg.get(), &assert_numbers<size_t, ALObjectPtr> },
    { Qfunction_arg.get(), &assert_function<size_t, ALObjectPtr> },
    { Qfile_arg.get(), &assert_file<size_t, ALObjectPtr> },
    { Qstream_arg.get(), &assert_stream<size_t, ALObjectPtr> },
    { Qmemory_arg.get(), &assert_memory<size_t, ALObjectPtr> },
    { Qbyte_arg.get(), &assert_byte<size_t, ALObjectPtr> },
    { Qbytearray_arg.get(), &assert_byte_array<size_t, ALObjectPtr> },
    { Qany_arg.get(), &ignore },
};


}  // namespace alisp
