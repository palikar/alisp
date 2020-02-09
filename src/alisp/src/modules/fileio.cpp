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


#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/utility/defines.hpp"
#include "alisp/utility/files.hpp"
#include "alisp/utility/string_utils.hpp"
#include <filesystem>

namespace alisp
{

namespace detail
{


#ifdef ALISP_WIN
inline constexpr auto separator = "\\";
#else
inline constexpr auto separator = "/";
#endif



ALObjectPtr Froot(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    namespace fs = std::filesystem;
    assert_size<0>(t_obj);
    return make_string(fs::current_path().root_path());
}

ALObjectPtr Fdirectories(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    ALObject::list_type entries;

    for (auto& entr : fs::directory_iterator(path->to_string())) {
        if (!entr.is_directory()) { continue; }
        entries.push_back(make_string(entr.path().string()));
    }

    return make_object(entries);
}

ALObjectPtr Fentries(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    ALObject::list_type entries;

    for (auto& entr : fs::directory_iterator(path->to_string())) {
        entries.push_back(make_string(entr.path().string()));
    }

    return make_object(entries);
}

ALObjectPtr Fglob(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    return Qnil;
}

ALObjectPtr Ftouch(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    std::fstream fs;
    fs.open(path->to_string(), std::ios::out);
    if (!fs.is_open()) {
        return Qnil;
    }
    fs.close();

    return Qt;
}

ALObjectPtr Fcopy(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto source = eval->eval(t_obj->i(0));
    auto target = eval->eval(t_obj->i(1));
    assert_string(source);
    assert_string(target);

    try {
        fs::copy(source->to_string(), target->to_string());
    } catch (...) {
        return Qnil;
    }

    return Qt;
}

ALObjectPtr Fmove(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto source = eval->eval(t_obj->i(0));
    auto target = eval->eval(t_obj->i(1));
    assert_string(source);
    assert_string(target);

    try {
        fs::rename(source->to_string(), target->to_string());
    } catch (...) {
        return Qnil;
    }

    return Qt;
}

ALObjectPtr Fmake_symlink(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto link = eval->eval(t_obj->i(0));
    auto target = eval->eval(t_obj->i(1));
    assert_string(link);
    assert_string(target);

    try {
        fs::create_symlink(target->to_string(), link->to_string());
    } catch (...) {
        return Qnil;
    }

    return Qt;
}

ALObjectPtr Fdelete(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    bool val = fs::remove(path->to_string());

    return val ? Qt : Qnil;
}

ALObjectPtr Fmkdir(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    bool val = fs::create_directory(path->to_string());

    return val ? Qt : Qnil;
}

ALObjectPtr Ftemp_file(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    return Qnil;
}

ALObjectPtr Fread_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(t_obj);

    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    if (!fs::exists(path->to_string())) {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string())) {
        return Qnil;
    }

    std::ifstream infile(path->to_string().c_str(), std::ios::in | std::ios::ate | std::ios::binary);
    if (!infile.is_open()) {
        return Qnil;
    }

    auto size = infile.tellg();
    infile.seekg(0, std::ios::beg);
    assert(size >= 0);

    std::vector<char> v(static_cast<size_t>(size));
    infile.read(&v[0], static_cast<std::streamsize>(size));

    ALObject::list_type bytes;
    for (auto& ch : v) { bytes.push_back(make_int(static_cast<int>(ch))); }

    return make_object(bytes);
}

ALObjectPtr Fread_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);



    if (!fs::exists(path->to_string())) {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string())) {
        return Qnil;
    }

    return make_string(utility::load_file(path->to_string()));
}

ALObjectPtr Fwrite_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto path = eval->eval(t_obj->i(0));
    auto text = eval->eval(t_obj->i(1));
    assert_string(path);
    assert_string(text);


    if (!fs::exists(path->to_string())) {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string())) {
        return Qnil;
    }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out);
    outfile << text->to_string();
    outfile.close();

    return Qt;
}

ALObjectPtr Fwrite_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto path = eval->eval(t_obj->i(0));
    auto bytes = eval->eval(t_obj->i(1));
    assert_string(path);
    assert_byte_array(bytes);

    if (!fs::exists(path->to_string())) {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string())) {
        return Qnil;
    }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out | std::ios_base::binary);
    if (outfile.is_open()) { return Qnil; }
    for (auto& b : *bytes) { outfile.put(static_cast<char>(b->to_int())); }
    outfile.close();

    return Qt;
}

ALObjectPtr Fappend_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto path = eval->eval(t_obj->i(0));
    auto text = eval->eval(t_obj->i(1));
    assert_string(path);
    assert_string(text);


    if (!fs::exists(path->to_string())) {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string())) {
        return Qnil;
    }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::app);
    outfile << text->to_string();
    outfile.close();

    return Qt;
}

ALObjectPtr Fappend_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<2>(t_obj);

    auto path = eval->eval(t_obj->i(0));
    auto bytes = eval->eval(t_obj->i(1));
    assert_string(path);
    assert_byte_array(bytes);

    if (!fs::exists(path->to_string())) {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string())) {
        return Qnil;
    }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out | std::ios_base::binary | std::ios_base::app);
    if (outfile.is_open()) { return Qnil; }
    for (auto& b : *bytes) { outfile.put(static_cast<char>(b->to_int())); }
    outfile.close();

    return Qt;
}

ALObjectPtr Fjoin(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_min_size<2>(t_obj);
    auto paths = eval_transform(eval, t_obj);
    auto path_1 = paths->i(0);
    assert_string(path_1);
    fs::path path = path_1->to_string();

    for (size_t i = 1; i < t_obj->size(); ++i) {
        auto path_n = t_obj->i(i);
        assert_string(path_n);
        path /= path_n->to_string();
    }

    return make_string(path.string());
}

ALObjectPtr Fsplit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;
    
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_string(path);

    auto parts = utility::split(path->to_string(), fs::path::preferred_separator);
    
    return make_list(parts);
}

ALObjectPtr Fexpand(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffilename(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdirname(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fcommon_parent(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fno_ext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fswap_ext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fbase(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Frelative(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fshort(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Flong(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fcanonical(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffull(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexists(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdirecotry(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffile(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fsymlink(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Freadable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fwritable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexecutable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fabsolute(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fprelative(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fis_Froot(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fexit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fsame(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fparent_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fchild_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fancestor_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fdescendant_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fhidden(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fempty(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

}  // namespace detail

env::ModulePtr init_fileio(env::Environment *, eval::Evaluator *)
{

    auto Mfileio = module_init("fileio");
    auto fio_ptr = Mfileio.get();

    module_defvar(fio_ptr, "directory-separator", make_string(detail::separator));


    return Mfileio;
}


}  // namespace alisp
