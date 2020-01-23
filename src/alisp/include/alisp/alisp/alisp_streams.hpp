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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/management/registry.hpp"
#include "alisp/streams/streams.hpp"

#include "alisp/config.hpp"

namespace alisp
{


namespace al
{

inline management::Registry<streams::ALStream*, STREAM_REGISTRY_TAG> streams_registry;

inline streams::ALStream& cout = *streams::CoutStream::get_instance();
inline streams::ALStream& cin = *streams::CinStream::get_instance();

inline uint32_t cout_id;
inline uint32_t cin_id;


void init_streams();
void reset_system_streams();
}

struct StreamsHelper {

  private:

    static uint32_t object_to_resource(ALObjectPtr t_obj);
    static ALObjectPtr resource_to_object(uint32_t t_id);
    
    
  public:

    static void rebind_cout(ALObjectPtr t_stream);
    static void rebind_cin(ALObjectPtr t_stream);

    static ALObjectPtr create_string_stream(ALObjectPtr t_string);
    static ALObjectPtr create_file_stream(ALObjectPtr t_file);

    static void close_stream(ALObjectPtr t_stream);
    
};

struct StreamClose {


    explicit StreamClose() {}
    ~StreamClose() {}

    StreamClose(StreamClose &&) = default;
    StreamClose& operator=(StreamClose &&) = default;
    StreamClose(const StreamClose &) = delete;

    
};




}

