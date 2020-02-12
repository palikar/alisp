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

#include "alisp/alisp/alisp_loadable_modules.hpp"
#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"


namespace alisp
{

namespace dynmoduels
{

AlispDynModule::AlispDynModule(const std::string &t_module_name, const std::string_view &t_filename)
  : m_dlmodule(t_filename), m_init_func(m_dlmodule, "init_" + t_module_name)
{
}

std::shared_ptr<env::Module> AlispDynModule::init_dynmod(env::Environment *env, eval::Evaluator *eval)
{
    return m_init_func.m_symbol(env, eval);
}

}  // namespace dynmoduels

}  // namespace alisp
