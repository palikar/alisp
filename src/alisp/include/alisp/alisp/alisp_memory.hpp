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

#include <functional>
#include <filesystem>
#include <fstream>
#include <utility>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/management/registry.hpp"

#include "alisp/utility/macros.hpp"

#include "alisp/config.hpp"

namespace alisp
{


namespace memory
{


struct MemoryBuffer
{
    char *m_ptr;
    size_t m_size;
};


inline management::Registry<MemoryBuffer, MEMORY_BUFFER_REGISTRY_TAG> memory_registry;

}  // namespace memory

struct MemoryHelpers
{
  public:
    static ALObjectPtr allocate_buffer(size_t t_size);
    static memory::MemoryBuffer &get_buffer(ALObjectPtr t_buffer);
    static void release_buffer(ALObjectPtr t_buffer);
};

struct BufferRelease
{

  private:
    ALObjectPtr m_id;

  public:
    explicit BufferRelease(ALObjectPtr t_id) : m_id(t_id) {}
    ~BufferRelease() { MemoryHelpers::release_buffer(m_id); }

    ALISP_RAII_OBJECT(BufferRelease);
};


}  // namespace alisp
