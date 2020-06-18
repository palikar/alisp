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
};


inline management::Registry<std::unique_ptr<FileObj, std::function<void(files::FileObj *)>>, FILE_REGISTRY_TAG>
  files_registry;

}  // namespace files


struct FileHelpers
{

  public:
    static ALObjectPtr open_file(const ALObjectPtr &t_file, const ALObjectPtr &t_output, const ALObjectPtr &t_input);

    static ALObjectPtr put_file(std::string t_path, std::fstream &&t_stream, bool t_input, bool t_output);

    static files::FileObj &get_file(ALObjectPtr t_file);

    static void close_file(const ALObjectPtr &t_file);

    static std::string temp_file_path(std::string t_prefix = "al");
};


struct FileClose
{

  private:
    ALObjectPtr m_id;

  public:
    explicit FileClose(ALObjectPtr t_id) : m_id(std::move(t_id)) {}
    ~FileClose() { FileHelpers::close_file(m_id); }

    ALISP_RAII_OBJECT(FileClose);
};


}  // namespace alisp
