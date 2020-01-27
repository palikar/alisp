#include "alisp/alisp/alisp_warnings.hpp"

namespace alisp
{

namespace warnings
{

void init_warning(std::vector<std::string_view> t_enabled_warning)
{

    for (auto &warn : t_enabled_warning)
    {

        switch (hash::hash(warn))
        {
        case hash::hash("all"): WarningsHelper::g_warning_bits |= WarningsHelper::ALL_BIT; break;

        case hash::hash("import"): WarningsHelper::g_warning_bits |= WarningsHelper::IMPORT_BIT; break;

        case hash::hash("deprecated"): WarningsHelper::g_warning_bits |= WarningsHelper::DEPRECATED_BIT; break;

        case hash::hash("user"): WarningsHelper::g_warning_bits |= WarningsHelper::USER_BIT; break;

        case hash::hash("math"): WarningsHelper::g_warning_bits |= WarningsHelper::MATH_BIT; break;

        default: std::cerr << "Unknown warning type: " << warn << "\n";
        }
    }
}

void warning(ALObjectPtr t_sym, std::string_view t_msg)
{
    if ((WarningsHelper::g_warning_bits & ~WarningsHelper::NONE_BIT) > 0) { return; }

    if ((WarningsHelper::g_warning_bits & ~WarningsHelper::USER_BIT) > 0 or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
    { std::cerr << "Warning[" << t_sym->to_string() << "]: " << t_msg << "\n"; }
}

void warning_internal(WarnTypes t_type, std::string_view t_msg)
{
    if ((WarningsHelper::g_warning_bits & ~WarningsHelper::NONE_BIT) > 0) { return; }

    switch (t_type)
    {

    case WarnTypes::IMPORT:
        if ((WarningsHelper::g_warning_bits & ~WarningsHelper::IMPORT_BIT) > 0 or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
        { std::cerr << "Warning[IMPORT]: " << t_msg << "\n"; }
        return;

    case WarnTypes::DEPRECATED:
        if ((WarningsHelper::g_warning_bits & ~WarningsHelper::DEPRECATED_BIT) > 0 or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
        { std::cerr << "Warning[DEPRECATED]: " << t_msg << "\n"; }
        return;

    case WarnTypes::MATH:
        if ((WarningsHelper::g_warning_bits & ~WarningsHelper::MATH_BIT) > 0 or (WarningsHelper::g_warning_bits & ~WarningsHelper::ALL_BIT) > 0)
        { std::cerr << "Warning[MATH]: " << t_msg << "\n"; }
        return;
    }
}

}  // namespace warnings

}  // namespace alisp
