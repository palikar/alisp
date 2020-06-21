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

#include "alisp/alisp/alisp_warnings.hpp"

namespace alisp
{

namespace warnings
{

void init_warning(std::vector<std::string> t_enabled_warning)
{
    AL_DEBUG("Initing the warnings"s);

    for (auto &warn : t_enabled_warning)
    {
        switch (hash::hash(warn))
        {
            case hash::hash("all"): WarningsHelper::g_warning_bits |= WarningsHelper::ALL_BIT; break;

            case hash::hash("import"): WarningsHelper::g_warning_bits |= WarningsHelper::IMPORT_BIT; break;

            case hash::hash("deprecated"): WarningsHelper::g_warning_bits |= WarningsHelper::DEPRECATED_BIT; break;

            case hash::hash("user"): WarningsHelper::g_warning_bits |= WarningsHelper::USER_BIT; break;

            case hash::hash("math"): WarningsHelper::g_warning_bits |= WarningsHelper::MATH_BIT; break;

            case hash::hash("eval"): WarningsHelper::g_warning_bits |= WarningsHelper::EVAL_BIT; break;

            case hash::hash("env"): WarningsHelper::g_warning_bits |= WarningsHelper::ENV_BIT; break;

            case hash::hash("common"): WarningsHelper::g_warning_bits |= WarningsHelper::COMMON_BIT; break;

            default: std::cerr << "Unknown warning type: " << warn << "\n";
        }
    }
}

void warning(const ALObjectPtr &t_sym, std::string_view t_msg)
{
    if ((WarningsHelper::g_warning_bits & ~WarningsHelper::NONE_BIT) > 0)
    {
        return;
    }

    if ((WarningsHelper::g_warning_bits & ~WarningsHelper::USER_BIT) > 0
        or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
    {
        std::cerr << "Warning[" << t_sym->to_string() << "]: " << t_msg << "\n";
    }
}

void warning_internal(WarnTypes t_type, std::string_view t_msg)
{
    if ((WarningsHelper::g_warning_bits & ~WarningsHelper::NONE_BIT) > 0)
    {
        return;
    }

    switch (t_type)
    {

        case WarnTypes::IMPORT:
            if ((WarningsHelper::g_warning_bits & ~WarningsHelper::IMPORT_BIT) > 0
                or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
            {
                std::cerr << "Warning[IMPORT]: " << t_msg << "\n";
            }
            return;

        case WarnTypes::DEPRECATED:
            if ((WarningsHelper::g_warning_bits & ~WarningsHelper::DEPRECATED_BIT) > 0
                or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
            {
                std::cerr << "Warning[DEPRECATED]: " << t_msg << "\n";
            }
            return;

        case WarnTypes::MATH:
            if ((WarningsHelper::g_warning_bits & ~WarningsHelper::MATH_BIT) > 0
                or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
            {
                std::cerr << "Warning[MATH]: " << t_msg << "\n";
            }
            return;

        case WarnTypes::ENV:
            if ((WarningsHelper::g_warning_bits & ~WarningsHelper::ENV_BIT) > 0
                or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
            {
                std::cerr << "Warning[ENV]: " << t_msg << "\n";
            }
            return;

        case WarnTypes::EVAL:
            if ((WarningsHelper::g_warning_bits & ~WarningsHelper::EVAL_BIT) > 0
                or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
            {
                std::cerr << "Warning[EVAL]: " << t_msg << "\n";
            }
            return;

        case WarnTypes::COMMON:
            if ((WarningsHelper::g_warning_bits & ~WarningsHelper::COMMON_BIT) > 0
                or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
            {
                std::cerr << "Warning[COMMON]: " << t_msg << "\n";
            }
            return;
    }
}

}  // namespace warnings

}  // namespace alisp
