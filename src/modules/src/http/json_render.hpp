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

#include "alisp/config.hpp"

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_asyncs.hpp"
#include "alisp/alisp/declarations/constants.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/utility.hpp"

#include <memory>
#include <vector>
#include <unordered_map>
#include <stdio.h>
#include <time.h>

#include <fmt/format.h>
#include <nlohmann/json.hpp>

namespace http::detail
{

using namespace alisp;

inline nlohmann::json json_render(ALObjectPtr &t_obj)
{
    if (t_obj == Qnil)
    {
        return {};
    }

    if (plist(t_obj))
    {
        nlohmann::json res;

        for (size_t i = 0; i < std::size(*t_obj); ++i)
        {
            if (psym(t_obj->i(i)))
            {
                auto name = utility::replace(t_obj->i(i)->to_string(), ":", "");
                res[name] = json_render(t_obj->i(i + 1));
                ++i;
                continue;
            }

            res.push_back(json_render(t_obj->i(i)));
        }
        return res;
    }

    if (pint(t_obj))
    {
        return t_obj->to_int();
    }

    if (preal(t_obj))
    {
        return t_obj->to_real();
    }

    if (pstring(t_obj))
    {
        return t_obj->to_string();
    }


    return {};
}


}  // namespace http::detail
