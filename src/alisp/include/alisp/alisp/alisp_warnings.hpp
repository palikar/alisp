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
#include <string_view>
#include <vector>
#include <cctype>
#include <iostream>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/utility/logging.hpp"

#include "alisp/utility/hash.hpp"

namespace alisp
{

namespace warnings
{

struct WarningsHelper
{
    inline static std::uint8_t g_warning_bits{ 0 };

    constexpr static std::uint8_t ALL_BIT        = 0x80;
    constexpr static std::uint8_t NONE_BIT       = 0x40;
    constexpr static std::uint8_t IMPORT_BIT     = 0x01;
    constexpr static std::uint8_t DEPRECATED_BIT = 0x02;
    constexpr static std::uint8_t USER_BIT       = 0x03;
    constexpr static std::uint8_t MATH_BIT       = 0x04;
    constexpr static std::uint8_t EVAL_BIT       = 0x08;
    constexpr static std::uint8_t ENV_BIT        = 0x10;
};

enum class WarnTypes
{
    IMPORT,
    DEPRECATED,
    MATH,
    EVAL,
    ENV,
};

void init_warning(std::vector<std::string> t_enabled_warning = {});

void warning(ALObjectPtr t_sym, std::string_view t_msg);

void warning_internal(WarnTypes t_type, std::string_view t_msg);

}  // namespace warnings

namespace warn{
inline void warn_math(std::string_view t_msg)
{
    warnings::warning_internal(warnings::WarnTypes::MATH, std::move(t_msg));
}

inline void warn_deprecated(std::string_view t_msg)
{
    warnings::warning_internal(warnings::WarnTypes::DEPRECATED, std::move(t_msg));
}

inline void warn_import(std::string_view t_msg)
{
    warnings::warning_internal(warnings::WarnTypes::IMPORT, std::move(t_msg));
}

inline void warn_eval(std::string_view t_msg)
{
    warnings::warning_internal(warnings::WarnTypes::EVAL, std::move(t_msg));
}

inline void warn_env(std::string_view t_msg)
{
    warnings::warning_internal(warnings::WarnTypes::ENV, std::move(t_msg));
}

}

}  // namespace alisp
