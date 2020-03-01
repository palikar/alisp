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


#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/utility.hpp"


namespace alisp
{

ALObjectPtr env::intern(std::string name)
{

    if (env::Environment::g_global_symbol_table.count(name))
    { return env::Environment::g_global_symbol_table.at(name); }

    if (env::Environment::g_symbol_table.count(name)) { return env::Environment::g_symbol_table.at(name); }

    auto [new_sym, insertion] = env::Environment::g_symbol_table.insert({ name, make_symbol(name) });

    return new_sym->second;
}

void env::update_prime(ALObjectPtr t_sym, ALObjectPtr t_val)
{
    env::Environment::g_prime_values.at(t_sym->to_string()) = std::move(t_val);
}


uint32_t object_to_resource(ALObjectPtr t_obj)
{
    return static_cast<uint32_t>(t_obj->to_int());
}

ALObjectPtr resource_to_object(uint32_t t_id)
{
    return make_int(static_cast<ALObject::int_type>(t_id));
}


}  // namespace alisp
