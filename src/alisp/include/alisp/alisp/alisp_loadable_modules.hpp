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

#include <dlfcn.h>

#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"

namespace alisp
{

namespace dynmoduels
{

struct DLModule
{
    explicit DLModule(const std::string_view &t_filename) : m_data(dlopen(t_filename.data(), RTLD_NOW))
    {
        if (m_data == nullptr) {}
    }

    DLModule(DLModule &&) = default;
    DLModule &operator=(DLModule &&) = default;
    DLModule(const DLModule &)       = delete;
    DLModule &operator=(const DLModule &) = delete;

    ~DLModule() { dlclose(m_data); }

    void *m_data;
};


template<typename T>
struct DLSym
{
    DLSym(DLModule &t_mod, const std::string_view &t_symbol)
        : m_symbol(reinterpret_cast<T>(dlsym(t_mod.m_data, t_symbol.data())))
    {
        if (!m_symbol)
        {
        }
    }

    T m_symbol;
};


struct AlispDynModule
{
    AlispDynModule(const std::string &t_module_name, const std::string_view &t_filename)
        : m_dlmodule(t_filename), m_init_func(m_dlmodule, "init_" + t_module_name)
    {}

    env::ModulePtr init_dynmod(env::Environment* env, eval::Evaluator* eval)
    {
        return m_init_func.m_symbol(env, eval);
    }
    
    DLModule m_dlmodule;
    DLSym<env::module_init_func> m_init_func;
};

}  // namespace dynmoduels

}  // namespace alisp
