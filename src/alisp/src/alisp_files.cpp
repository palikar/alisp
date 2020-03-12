#include "alisp/alisp/alisp_files.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_object.hpp"

#include <functional>

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

    auto file_ptr = std::unique_ptr<files::FileObj, std::function<void(files::FileObj *)>>(
      new files::FileObj{ fs::path(t_file->to_string()), std::move(file_stream), output, input },
      [](files::FileObj *ptr) {
          if (ptr->m_file.is_open()) { ptr->m_file.close(); }
          delete ptr;
      });

    auto new_id  = files::files_registry.emplace_resource(std::move(file_ptr))->id;
    auto new_obj = resource_to_object(new_id);

    new_obj->set_prop("file-path", t_file);
    new_obj->set_prop("file-output", t_output);
    new_obj->set_prop("file-input", t_input);

    AL_DEBUG("Opening a new file: "s += t_file->to_string());

    return new_obj;
}

ALObjectPtr FileHelpers::put_file(std::string t_path, std::fstream &&t_stream, bool t_input, bool t_output)
{
    namespace fs  = std::filesystem;
    auto file_ptr = new files::FileObj{ fs::path(t_path), std::move(t_stream), t_output, t_input };

    auto new_id  = files::files_registry.emplace_resource(file_ptr)->id;
    auto new_obj = resource_to_object(new_id);

    new_obj->set_prop("file-path", make_string(t_path));
    new_obj->set_prop("file-output", t_output ? Qt : Qnil);
    new_obj->set_prop("file-input", t_input ? Qt : Qnil);

    AL_DEBUG("Putting a new file: "s += t_path);

    return new_obj;
}

std::string FileHelpers::temp_file_path(std::string t_prefix)
{
    for (int count = 0; count < 1000; ++count)
    {
        const auto p =
          std::filesystem::temp_directory_path()
          / fmt::format(
            "{}-{}-{:04x}", t_prefix, std::chrono::system_clock::to_time_t(std::chrono::system_clock::now()), count);
        if (!std::filesystem::exists(p)) { return p; }
    }

    return "";
}

files::FileObj &FileHelpers::get_file(ALObjectPtr t_file)
{
    return *files::files_registry[object_to_resource(t_file)];
}

void FileHelpers::close_file(ALObjectPtr t_file)
{
    const auto id = object_to_resource(t_file);
    auto file     = files::files_registry[id].release();
    AL_DEBUG("Closing file: "s += file->m_path);
    file->m_file.close();
    delete file;
    files::files_registry.destroy_resource(id);
}

}  // namespace alisp
