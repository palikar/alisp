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


namespace files
{

namespace fs = std::filesystem;

struct FileObj
{
    fs::path m_path;
    std::fstream m_file;
    bool m_output;
    bool m_input;

    // FileObj(FileObj&& o) {
    //     m_path = std::move(o.m_path);
    //     m_file = std::move(o.m_file);
    //     m_output = o.m_output;
    //     m_input = o.m_input;
    // }
};


inline management::Registry<FileObj, FILE_REGISTRY_TAG> files_registry;

}



struct FileHelpers
{
  private:
    static uint32_t object_to_resource(ALObjectPtr t_obj);
    static ALObjectPtr resource_to_object(uint32_t t_id);

  public:

    static ALObjectPtr open_file(ALObjectPtr t_file, ALObjectPtr t_output, ALObjectPtr t_input);

    static void close_file(ALObjectPtr t_file);
    
};


struct FileClose
{

  private:
    ALObjectPtr m_id;

  public:

    explicit FileClose(ALObjectPtr t_id) : m_id(t_id) {}
    ~FileClose() { FileHelpers::close_file(m_id); }

    ALISP_RAII_OBJECT(FileClose);
};


}
