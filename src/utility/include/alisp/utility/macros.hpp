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


#if (defined(__GNUC__) && __GNUC__ >= 3)
#define ALISP_UNLIKELY(x) (__builtin_expect(x, 0))
#define ALISP_LIKELY(x) (__builtin_expect(!!(x), 1))
#else
#define ALISP_UNLIKELY(x) (x)
#define ALISP_LIKELY(x) (x)
#endif


#define ALISP_DISALLOW_COPY_ASSIGN(TypeName) \
    TypeName(const TypeName &) = delete;     \
    void operator=(const TypeName &) = delete

#define ALISP_DISALLOW_COPY_ASSIGN_MOVE(TypeName) \
    TypeName(const TypeName &) = delete;          \
    TypeName(TypeName &&)      = delete;          \
    void operator=(const TypeName &) = delete;    \
    void operator=(TypeName &&) = delete


#if DEBUG_LOGGING
#define ALISP_HERE(message)                                                                                       \
    do                                                                                                            \
    {                                                                                                             \
        std::cout << "-> here() called in " << __FILE__ << " line " << __LINE__ << ". " << #message << std::endl; \
    } while (0);
#else
#define ALISP_HERE(message) (void)0
#endif
