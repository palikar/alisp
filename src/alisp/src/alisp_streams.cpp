#include "alisp/alisp/alisp_streams.hpp"
#include "alisp/alisp/alisp_factory.hpp"


namespace alisp
{

namespace al
{

void init_streams() {
    cout_id = streams_registry.put_resource(dynamic_cast<streams::ALStream*>(streams::CoutStream::get_instance()))->id;
    cin_id = streams_registry.put_resource(dynamic_cast<streams::ALStream*>(streams::CinStream::get_instance()))->id;
}

void reset_system_streams() {
    cout = *streams_registry[cout_id];
    cin = *streams_registry[cin_id];
}

}

uint32_t StreamsHelper::object_to_resource(ALObjectPtr t_obj) {
    return static_cast<uint32_t>(t_obj->to_int());
}

ALObjectPtr StreamsHelper::resource_to_object(uint32_t t_id) {
    return make_int(static_cast<ALObject::int_type>(t_id));
}

void StreamsHelper::rebind_cout(ALObjectPtr t_stream) {
    const auto id = object_to_resource(t_stream);
    al::cout = *al::streams_registry[id];
}

void StreamsHelper::rebind_cin(ALObjectPtr t_stream) {
    const auto id = object_to_resource(t_stream);
    al::cin = *al::streams_registry[id];
}

streams::ALStream* StreamsHelper::get_stream(ALObjectPtr t_stream) {
    return al::streams_registry[object_to_resource(t_stream)];
}

ALObjectPtr StreamsHelper::create_string_stream(ALObjectPtr t_string) {
    streams::StringStream* new_stream = new streams::StringStream(t_string->to_string());
    auto new_id = al::streams_registry.put_resource(dynamic_cast<streams::ALStream*>(new_stream))->id;
    return resource_to_object(new_id);
}

ALObjectPtr StreamsHelper::create_file_stream(ALObjectPtr) {

    return nullptr;
}

void StreamsHelper::close_stream(ALObjectPtr t_stream) {
    const auto id = object_to_resource(t_stream);
    streams::ALStream* old_stream = al::streams_registry[id];
    delete old_stream;
    al::streams_registry.destroy_resource(id);
}

}
