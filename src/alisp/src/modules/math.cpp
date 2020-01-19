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




namespace alisp
{

namespace detail
{

inline constexpr double PI = 3.14159265358979323846;
inline constexpr double E = 2.71828182845904523536;
inline constexpr double TAU = 2*PI;

}


env::ModulePtr init_math(env::Environment*, eval::Evaluator*) {

    auto Mmath = module_init("math");

    module_defvar(Mmath.get(), "PI", make_double(detail::PI));
    module_defvar(Mmath.get(), "E", make_double(detail::E));
    module_defvar(Mmath.get(), "TAU", make_double(detail::TAU));



    return Mmath;
}


}
