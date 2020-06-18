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


#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshadow"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wnon-virtual-dtor"
// #pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include <cpr/cpr.h>

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/utility/files.hpp"
#include "alisp/utility/string_utils.hpp"

namespace request
{

}  // namespace request

ALISP_EXPORT alisp::env::ModulePtr init_xml(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mrequest = alisp::module_init("request");
    auto req_ptr  = Mrequest.get();


    return Mrequest;
}
