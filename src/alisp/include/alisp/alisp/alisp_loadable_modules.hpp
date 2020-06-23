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

#include <string_view>
#include <string>
#include <utility>
#include <memory>
#include <iostream>


#ifdef ALISP_WIN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

namespace alisp
{

namespace env
{
class Environment;
}
namespace env
{
class Module;
}
namespace eval
{
class Evaluator;
}

using module_init_func = std::shared_ptr<env::Module> (*)(env::Environment *, eval::Evaluator *);

namespace dynmoduels
{


#ifdef ALISP_WIN
template<typename T> static std::wstring to_wstring(const T &t_str)
{
    return std::wstring(t_str.begin(), t_str.end());
}

template<typename T> static std::string to_string(const T &t_str)
{
    return std::string(t_str.begin(), t_str.end());
}

#if defined(_UNICODE) || defined(UNICODE)
template<typename T> static std::wstring to_proper_string(const T &t_str)
{
    return to_wstring(t_str);
}
#else
template<typename T> static std::string to_proper_string(const T &t_str)
{
    return to_string(t_str);
}
#endif


template<typename T> struct DLSym
{
    DLSym(DLModule &t_mod, const std::string &t_symbol)
      : m_symbol(reinterpret_cast<T>(GetProcAddress(t_mod.m_data, t_symbol.c_str())))
    {
        if (!m_symbol)
        {
        }
    }

    T m_symbol;
};

struct DLModule
{
    explicit DLModule(const std::string &t_filename) : m_data(LoadLibrary(to_proper_string(t_filename).c_str()))
    {
        if (!m_data)
        {
        }
    }

    DLModule(DLModule &&) = default;
    DLModule &operator=(DLModule &&) = default;
    DLModule(const DLModule &)       = delete;
    DLModule &operator=(const DLModule &) = delete;

    ~DLModule() { FreeLibrary(m_data); }

    HMODULE m_data;
};

#else

struct DLModule
{
    explicit DLModule(const std::string_view &t_filename);

    DLModule(DLModule &&) = default;
    DLModule &operator=(DLModule &&) = default;
    DLModule(const DLModule &)       = delete;
    DLModule &operator=(const DLModule &) = delete;

    // ~DLModule() { dlclose(m_data); }

    void *m_data;
};


template<typename T> struct DLSym
{
    DLSym(DLModule &t_mod, const std::string_view &t_symbol)
      : m_symbol(reinterpret_cast<T>(dlsym(t_mod.m_data, t_symbol.data())))
    {
    }

    T m_symbol;
};

#endif


struct AlispDynModule
{
    AlispDynModule(const std::string &t_module_name, const std::string_view &t_filename);

    std::shared_ptr<env::Module> init_dynmod(env::Environment *env, eval::Evaluator *eval);

    DLModule m_dlmodule;
    DLSym<module_init_func> m_init_func;
};

}  // namespace dynmoduels

}  // namespace alisp
