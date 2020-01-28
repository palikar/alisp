#include "alisp/alisp/alisp_files.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_declarations.hpp"

namespace alisp
{


uint32_t FileHelpers::object_to_resource(ALObjectPtr t_obj)
{
    return static_cast<uint32_t>(t_obj->to_int());
}

ALObjectPtr FileHelpers::resource_to_object(uint32_t t_id)
{
    return make_int(static_cast<ALObject::int_type>(t_id));
}

ALObjectPtr FileHelpers::open_file(ALObjectPtr t_file, ALObjectPtr t_output, ALObjectPtr t_input)
{
    namespace fs = std::filesystem;

    bool output = t_output == Qt;
    bool input  = t_input == Qt;

    auto mode = [&]() {
        if (input and output) return std::ios::out | std::ios::in;
        else if (input)
            return std::ios::in;
        else
            return std::ios::out;
    }();

    std::fstream file_stream{ t_file->to_string(), mode };
    
    auto new_id  = files::files_registry.emplace_resource(fs::path{t_file->to_string()}, std::move(file_stream), output, input)->id;
    auto new_obj = resource_to_object(new_id);

    new_obj->set_prop("file-path", t_file);
    new_obj->set_prop("file-output", t_output);
    new_obj->set_prop("file-input", t_input);

    return new_obj;
}

void FileHelpers::close_file(ALObjectPtr t_file)
{
    const auto id = object_to_resource(t_file);
    auto &file    = files::files_registry[id];
    file.m_file.flush();
    file.m_file.close();
    files::files_registry.destroy_resource(id);
}

}  // namespace alisp
