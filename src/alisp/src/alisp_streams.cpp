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

#include "alisp/alisp/alisp_streams.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_files.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_object.hpp"


namespace alisp
{

namespace al
{

void init_streams()
{
    AL_DEBUG("Initing the streams. Binding cout and cin"s);

    auto cout_stream = std::unique_ptr<streams::ALStream, std::function<void(streams::ALStream *)>>(
      dynamic_cast<streams::ALStream *>(streams::CoutStream::get_instance()), [](streams::ALStream *) {});

    auto cerr_stream = std::unique_ptr<streams::ALStream, std::function<void(streams::ALStream *)>>(
      dynamic_cast<streams::ALStream *>(streams::CerrStream ::get_instance()), [](streams::ALStream *) {});

    auto cin_stream = std::unique_ptr<streams::ALStream, std::function<void(streams::ALStream *)>>(
      dynamic_cast<streams::ALStream *>(streams::CinStream::get_instance()), [](streams::ALStream *) {});

    cout_id = streams_registry.emplace_resource(std::move(cout_stream))->id;
    cerr_id = streams_registry.emplace_resource(std::move(cerr_stream))->id;
    cin_id  = streams_registry.emplace_resource(std::move(cin_stream))->id;
}

void reset_system_streams()
{
    cout = *streams_registry[cout_id];
    cerr = *streams_registry[cerr_id];
    cin  = *streams_registry[cin_id];
}

}  // namespace al

void StreamsHelper::rebind_cout(ALObjectPtr t_stream)
{
    AL_DEBUG("Rebinging cout."s);
    const auto id = object_to_resource(t_stream);
    al::cout      = *al::streams_registry[id];
}

void StreamsHelper::rebind_cerr(ALObjectPtr t_stream)
{
    AL_DEBUG("Rebinging cerr."s);
    const auto id = object_to_resource(t_stream);
    al::cerr      = *al::streams_registry[id];
}


void StreamsHelper::rebind_cin(ALObjectPtr t_stream)
{
    AL_DEBUG("Rebinging cin."s);
    const auto id = object_to_resource(t_stream);
    al::cin       = *al::streams_registry[id];
}

streams::ALStream *StreamsHelper::get_stream(ALObjectPtr t_stream)
{
    return al::streams_registry[object_to_resource(t_stream)].get();
}

ALObjectPtr StreamsHelper::create_string_stream(ALObjectPtr t_string)
{
    streams::StringStream *new_stream = new streams::StringStream(t_string->to_string());

    auto string_stream = std::unique_ptr<streams::ALStream, std::function<void(streams::ALStream *)>>(
      dynamic_cast<streams::ALStream *>(new_stream), [](streams::ALStream *ptr) { delete ptr; });

    auto new_id = al::streams_registry.emplace_resource(std::move(string_stream))->id;
    AL_DEBUG("New string stream: "s += std::to_string(new_id));
    return resource_to_object(new_id);
}

ALObjectPtr StreamsHelper::create_file_stream(ALObjectPtr t_file)
{
    streams::FileStream *new_stream = new streams::FileStream(FileHelpers::get_file(t_file).m_file);

    auto file_stream = std::unique_ptr<streams::ALStream, std::function<void(streams::ALStream *)>>(
      dynamic_cast<streams::ALStream *>(new_stream), [](streams::ALStream *ptr) { delete ptr; });

    auto new_id = al::streams_registry.emplace_resource(std::move(file_stream))->id;
    AL_DEBUG("New file stream: "s += std::to_string(new_id));
    return resource_to_object(new_id);
}

void StreamsHelper::close_stream(ALObjectPtr t_stream)
{
    const auto id = object_to_resource(t_stream);
    AL_DEBUG("Closing stream: "s += std::to_string(id));
    streams::ALStream *old_stream = al::streams_registry[id].release();
    delete old_stream;
    al::streams_registry.destroy_resource(id);
}

}  // namespace alisp
