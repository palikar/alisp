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

#include <string>

#include "defines.hpp"

#ifdef ALISP_WIN
#include "./windows/system.hpp"
#else
#include "./linux/system.hpp"
#endif


namespace alisp::utility
{

template<typename OS> struct SystemTemplate
{


    static std::string executable() { return OS::executable(); }
};


#ifdef ALISP_WIN
using System = SystemTemplate<WindowsSystem>;
#else
using System = SystemTemplate<LinuxSystem>;
#endif

}  // namespace alisp::utility
