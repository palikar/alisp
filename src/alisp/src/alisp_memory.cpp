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


#include "alisp/alisp/alisp_memory.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_object.hpp"

#include <stdlib.h>

namespace alisp
{


ALObjectPtr MemoryHelpers::allocate_buffer(size_t t_size)
{
    unsigned char *memory = static_cast<unsigned char *>(malloc(t_size));
    auto new_id           = memory::memory_registry.put_resource({ memory, t_size })->id;
    AL_DEBUG("New memory buffer: " + std::to_string(new_id));
    return resource_to_object(new_id);
}

memory::MemoryBuffer &MemoryHelpers::get_buffer(ALObjectPtr t_buffer)
{

    return memory::memory_registry[object_to_resource(t_buffer)];
}


void MemoryHelpers::release_buffer(ALObjectPtr t_buffer)
{
    const auto id = object_to_resource(t_buffer);
    AL_DEBUG("Releasing buffer: " + std::to_string(id));
    auto buff     = memory::memory_registry[id];
    free(buff.m_ptr);
    memory::memory_registry.destroy_resource(id);
}


}  // namespace alisp
