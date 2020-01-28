#include "alisp/alisp/alisp_files.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_object.hpp"

namespace alisp
{


ALObjectPtr FileHelpers::open_file(ALObjectPtr t_file, ALObjectPtr t_output, ALObjectPtr t_input)
{
    namespace fs = std::filesystem;

    bool output = t_output == Qt;
    bool input  = t_input == Qt;

    auto mode = [&]() {
        if (input and output)
            return std::ios::out | std::ios::in;
        else if (input)
            return std::ios::in;
        else
            return std::ios::out;
    }();

    std::fstream file_stream{ t_file->to_string(), mode };

    auto file_ptr = new files::FileObj{ fs::path(t_file->to_string()), std::move(file_stream), output, input };

    auto new_id  = files::files_registry.emplace_resource(file_ptr)->id;
    auto new_obj = resource_to_object(new_id);

    new_obj->set_prop("file-path", t_file);
    new_obj->set_prop("file-output", t_output);
    new_obj->set_prop("file-input", t_input);

    return new_obj;
}

files::FileObj &FileHelpers::get_file(ALObjectPtr t_file)
{
    return *files::files_registry[object_to_resource(t_file)];
}

void FileHelpers::close_file(ALObjectPtr t_file)
{
    const auto id = object_to_resource(t_file);
    auto file     = files::files_registry[id];
    file->m_file.close();
    delete file;
    files::files_registry.destroy_resource(id);
}

}  // namespace alisp
