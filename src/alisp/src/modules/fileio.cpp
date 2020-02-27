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
#include <glob.h>
#include <string.h>

namespace alisp
{

namespace detail
{

std::vector<std::string> glob(const std::string &pattern)
{
    using namespace std;

    // glob struct resides on the stack
    glob_t glob_result;
    memset(&glob_result, 0, sizeof(glob_result));

    // do the glob operation
    int return_value = glob(pattern.c_str(), GLOB_TILDE, NULL, &glob_result);
    if (return_value != 0)
    {
        globfree(&glob_result);
        stringstream ss;
        ss << "glob() failed with return_value " << return_value << endl;
        throw std::runtime_error(ss.str());
    }

    // collect all the filenames into a std::list<std::string>
    vector<string> filenames;
    for (size_t i = 0; i < glob_result.gl_pathc; ++i) { filenames.push_back(string(glob_result.gl_pathv[i])); }

    // cleanup
    globfree(&glob_result);

    // done
    return filenames;
}

std::string expand_user(std::string path)
{
    if (not path.empty() and path[0] == '~')
    {
        assert(path.size() == 1 or path[1] == '/');  // or other error handling
        char const *home = getenv("HOME");
        if (home or ((home = getenv("USERPROFILE")))) { path.replace(0, 1, home); }
        else
        {
            char const *hdrive = getenv("HOMEDRIVE"), *hpath = getenv("HOMEPATH");
            assert(hdrive);  // or other error handling
            assert(hpath);
            path.replace(0, 1, std::string(hdrive) + hpath);
        }
    }
    return path;
}

#ifdef ALISP_WIN
inline constexpr auto separator = "\\";
#else
inline constexpr auto separator = "/";
#endif


ALObjectPtr Froot(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    namespace fs = std::filesystem;
    AL_CHECK(assert_size<0>(t_obj));
    return make_string(fs::current_path().root_path());
}

ALObjectPtr Fdirectories(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    ALObject::list_type entries;

    for (auto &entr : fs::directory_iterator(path->to_string()))
    {
        if (!entr.is_directory()) { continue; }
        entries.push_back(make_string(entr.path().string()));
    }

    return make_object(entries);
}

ALObjectPtr Fentries(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    ALObject::list_type entries;

    for (auto &entr : fs::directory_iterator(path->to_string())) { entries.push_back(make_string(entr.path().string())); }

    return make_object(entries);
}

ALObjectPtr Fglob(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_min_size<2>(t_obj));
    auto pattern = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(pattern));
    if (std::size(*t_obj) > 1)
    {
        auto path = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(path));
        return make_list(glob(path->to_string() + fs::path::preferred_separator + pattern->to_string()));
    }

    return make_list(glob(pattern->to_string()));
}

ALObjectPtr Ftouch(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    std::fstream fs;
    fs.open(path->to_string(), std::ios::out | std::ios::app);
    if (!fs.is_open()) { return Qnil; }
    fs.close();

    return Qt;
}

ALObjectPtr Fexpand_user(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    return make_string(expand_user(path->to_string()));
}

ALObjectPtr Fcopy(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto source = eval->eval(t_obj->i(0));
    auto target = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(source));
    AL_CHECK(assert_string(target));

    try
    {
        fs::copy(source->to_string(), target->to_string());
    }
    catch (...)
    {
        return Qnil;
    }

    return Qt;
}

ALObjectPtr Fmove(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto source = eval->eval(t_obj->i(0));
    auto target = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(source));
    AL_CHECK(assert_string(target));

    try
    {
        fs::rename(source->to_string(), target->to_string());
    }
    catch (...)
    {
        return Qnil;
    }

    return Qt;
}

ALObjectPtr Fmake_symlink(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto link   = eval->eval(t_obj->i(0));
    auto target = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(link));
    AL_CHECK(assert_string(target));

    try
    {
        fs::create_symlink(target->to_string(), link->to_string());
    }
    catch (...)
    {
        return Qnil;
    }

    return Qt;
}

ALObjectPtr Fdelete(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    bool val = fs::remove(path->to_string());

    return val ? Qt : Qnil;
}

ALObjectPtr Fmkdir(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    bool val = fs::create_directory(path->to_string());

    return val ? Qt : Qnil;
}

ALObjectPtr Fwith_temp_file(ALObjectPtr t_obj, env::Environment *env, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_min_size<1>(t_obj));
    auto sym = eval->eval(t_obj->i(0));
    AL_CHECK(assert_symbol(sym));

    auto path = FileHelpers::temp_file_path();
    auto id   = FileHelpers::put_file(path, std::fstream(path, std::ios::out), false, true);

    env::detail::ScopePushPop scope{ *env };
    env->put(sym, id);

    auto res = eval_list(eval, t_obj, 1);
    FileHelpers::close_file(id);
    fs::remove(path);
    return res;
}

ALObjectPtr Ftemp_file_name(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));
    return make_string(FileHelpers::temp_file_path());
}

ALObjectPtr Ftemp_file(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));
    auto path = FileHelpers::temp_file_path();
    return FileHelpers::put_file(path, std::fstream(path, std::ios::out), false, true);
}

ALObjectPtr Fread_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));

    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    if (!fs::exists(path->to_string())) { return Qnil; }
    if (!fs::is_regular_file(path->to_string())) { return Qnil; }

    std::ifstream infile(path->to_string().c_str(), std::ios::in | std::ios::ate | std::ios::binary);
    if (!infile.is_open()) { return Qnil; }

    auto size = infile.tellg();
    infile.seekg(0, std::ios::beg);
    assert(size >= 0);

    std::vector<char> v(static_cast<size_t>(size));
    infile.read(&v[0], static_cast<std::streamsize>(size));

    ALObject::list_type bytes;
    for (auto &ch : v) { bytes.push_back(make_int(static_cast<int>(ch))); }

    return make_object(bytes);
}

ALObjectPtr Fread_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));


    if (!fs::exists(path->to_string())) { return Qnil; }
    if (!fs::is_regular_file(path->to_string())) { return Qnil; }

    return make_string(utility::load_file(path->to_string()));
}

ALObjectPtr Fwrite_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto path = eval->eval(t_obj->i(0));
    auto text = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_string(text));


    if (!fs::exists(path->to_string())) { return Qnil; }
    if (!fs::is_regular_file(path->to_string())) { return Qnil; }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out);
    outfile << text->to_string();
    outfile.close();

    return Qt;
}

ALObjectPtr Fwrite_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto path  = eval->eval(t_obj->i(0));
    auto bytes = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_byte_array(bytes));

    if (!fs::exists(path->to_string())) { return Qnil; }
    if (!fs::is_regular_file(path->to_string())) { return Qnil; }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out | std::ios_base::binary);
    if (outfile.is_open()) { return Qnil; }
    for (auto &b : *bytes) { outfile.put(static_cast<char>(b->to_int())); }
    outfile.close();

    return Qt;
}

ALObjectPtr Fappend_text(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto path = eval->eval(t_obj->i(0));
    auto text = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_string(text));


    if (!fs::exists(path->to_string())) { return Qnil; }
    if (!fs::is_regular_file(path->to_string())) { return Qnil; }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::app);
    outfile << text->to_string();
    outfile.close();

    return Qt;
}

ALObjectPtr Fappend_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto path  = eval->eval(t_obj->i(0));
    auto bytes = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_byte_array(bytes));

    if (!fs::exists(path->to_string())) { return Qnil; }
    if (!fs::is_regular_file(path->to_string())) { return Qnil; }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out | std::ios_base::binary | std::ios_base::app);
    if (outfile.is_open()) { return Qnil; }
    for (auto &b : *bytes) { outfile.put(static_cast<char>(b->to_int())); }
    outfile.close();

    return Qt;
}

ALObjectPtr Fjoin(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_min_size<2>(t_obj));
    auto paths  = eval_transform(eval, t_obj);
    auto path_1 = paths->i(0);
    AL_CHECK(assert_string(path_1));
    fs::path path = path_1->to_string();

    for (size_t i = 1; i < t_obj->size(); ++i)
    {
        auto path_n = t_obj->i(i);
        AL_CHECK(assert_string(path_n));
        path /= path_n->to_string();
    }

    return make_string(path.string());
}

ALObjectPtr Fsplit(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    auto parts = utility::split(path->to_string(), fs::path::preferred_separator);

    return make_list(parts);
}

ALObjectPtr Fexpand(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::absolute(path->to_string());

    return make_string(p);
}

ALObjectPtr Ffilename(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::path(path->to_string());

    return make_string(p.filename());
}

ALObjectPtr Fdirname(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::path(path->to_string());

    return make_string(p.parent_path());
}

ALObjectPtr Fcommon_parent(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    return Qnil;
}

ALObjectPtr Fext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::path(path->to_string());

    return make_string(p.extension());
}

ALObjectPtr Fno_ext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::path(path->to_string());

    return make_string(p.stem());
}

ALObjectPtr Fswap_ext(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    auto ext  = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_string(ext));

    const auto p = fs::path(path->to_string());

    return make_string(p.stem().string() + "." + ext->to_string());
}

ALObjectPtr Fbase(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    auto ext  = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_string(ext));

    const auto p = fs::path(path->to_string());
    if (fs::is_directory(p)) { return Qnil; }

    return make_string(p.stem().filename());
}

ALObjectPtr Frelative(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_min_size<2>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::path(path->to_string());

    if (std::size(*t_obj) > 1)
    {
        auto to_path = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(to_path));
        return make_string(fs::relative(path->to_string(), to_path->to_string()));
    }

    return make_string(path->to_string());
}

ALObjectPtr Fshort(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    return Qnil;
}

ALObjectPtr Flong(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    return make_string(fs::canonical(path->to_string()));
}

ALObjectPtr Fcanonical(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    return make_string(fs::canonical(path->to_string()));
}

ALObjectPtr Ffull(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return make_string(fs::absolute(p));
}

ALObjectPtr Fexists(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::exists(p) ? Qt : Qnil;
}

ALObjectPtr Fdirecotry(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::is_directory(p) ? Qt : Qnil;
}

ALObjectPtr Ffile(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::is_regular_file(p) ? Qt : Qnil;
}

ALObjectPtr Fsymlink(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::is_symlink(p) ? Qt : Qnil;
}

ALObjectPtr Freadable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());

    return (fs::status(p).permissions() & fs::perms::owner_read) != fs::perms::none ? Qt : Qnil;
}

ALObjectPtr Fwritable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());

    return (fs::status(p).permissions() & fs::perms::owner_write) != fs::perms::none ? Qt : Qnil;
}

ALObjectPtr Fexecutable(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());

    return (fs::status(p).permissions() & fs::perms::owner_exec) != fs::perms::none ? Qt : Qnil;
}

ALObjectPtr Fabsolute(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return p.is_absolute() ? Qt : Qnil;
}

ALObjectPtr Fprelative(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return p.is_relative() ? Qt : Qnil;
}

ALObjectPtr Fis_root(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::equivalent(p, fs::current_path().root_path()) ? Qt : Qnil;
}

ALObjectPtr Fsame(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = fs::path(path1->to_string());
    const auto p2 = fs::path(path2->to_string());
    return fs::equivalent(p1, p2) ? Qt : Qnil;
}

ALObjectPtr Fparent_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = fs::path(path1->to_string());
    const auto p2 = fs::path(path2->to_string());
    return fs::equivalent(p1, p2.parent_path()) ? Qt : Qnil;
}

ALObjectPtr Fchild_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = fs::path(path1->to_string());
    const auto p2 = fs::path(path2->to_string());
    return fs::equivalent(p1, p2.parent_path()) ? Qt : Qnil;
}

ALObjectPtr Fancestor_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = (path1->to_string());
    const auto p2 = (path2->to_string());

    auto parts1 = utility::split(p2, fs::path::preferred_separator);
    auto parts2 = utility::split(p1, fs::path::preferred_separator);

    for (size_t i = 0; i < std::size(parts1); ++i)
    {

        if (std::size(parts2) <= i) { return Qnil; }

        if (parts2[i] != parts1[i]) { return Qnil; }
    }

    return Qt;
}

ALObjectPtr Fdescendant_of(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = (path1->to_string());
    const auto p2 = (path2->to_string());

    auto parts1 = utility::split(p1, fs::path::preferred_separator);
    auto parts2 = utility::split(p2, fs::path::preferred_separator);

    for (size_t i = 0; i < std::size(parts1); ++i)
    {

        if (std::size(parts2) <= i) { return Qnil; }

        if (parts2[i] != parts1[i]) { return Qnil; }
    }

    return Qt;
}

ALObjectPtr Fhidden(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;
    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    const auto p = fs::path(path->to_string());
    return p.filename().string()[0] == '.' ? Qt : Qnil;
}

ALObjectPtr Fempty(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;
    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    const auto p = fs::path(path->to_string());

    return fs::is_empty(p) ? Qt : Qnil;
}

}  // namespace detail

env::ModulePtr init_fileio(env::Environment *, eval::Evaluator *)
{

    auto Mfileio = module_init("fileio");
    auto fio_ptr = Mfileio.get();

    module_doc(fio_ptr, R"(The `fileio` moudule provides utilities for working with file paths, files, directories and some basic IO functions.)");

    module_defvar(fio_ptr, "f-directory-separator", make_string(detail::separator));

    module_defun(fio_ptr, "f-with-temp-file", &detail::Fwith_temp_file);
    module_defun(fio_ptr, "f-temp-file-name", &detail::Ftemp_file_name);
    module_defun(fio_ptr, "f-temp-file", &detail::Ftemp_file);
    module_defun(fio_ptr, "f-expand-user", &detail::Fexpand_user);
    module_defun(fio_ptr, "f-root", &detail::Froot);
    module_defun(fio_ptr, "f-directories", &detail::Fdirectories);
    module_defun(fio_ptr, "f-entries", &detail::Fentries);
    module_defun(fio_ptr, "f-glob", &detail::Fglob);
    module_defun(fio_ptr, "f-touch", &detail::Ftouch);
    module_defun(fio_ptr, "f-copy", &detail::Fcopy);
    module_defun(fio_ptr, "f-move", &detail::Fmove);
    module_defun(fio_ptr, "f-make-symlink", &detail::Fmake_symlink);
    module_defun(fio_ptr, "f-delete", &detail::Fdelete);
    module_defun(fio_ptr, "f-mkdir", &detail::Fmkdir);
    module_defun(fio_ptr, "f-read-bytes", &detail::Fread_bytes);
    module_defun(fio_ptr, "f-read-text", &detail::Fread_text);
    module_defun(fio_ptr, "f-write-text", &detail::Fwrite_text);
    module_defun(fio_ptr, "f-write-bytes", &detail::Fwrite_bytes);
    module_defun(fio_ptr, "f-append-text", &detail::Fappend_text);
    module_defun(fio_ptr, "f-append-bytes", &detail::Fappend_bytes);
    module_defun(fio_ptr, "f-join", &detail::Fjoin);
    module_defun(fio_ptr, "f-split", &detail::Fsplit);
    module_defun(fio_ptr, "f-expand", &detail::Fexpand);
    module_defun(fio_ptr, "f-filename", &detail::Ffilename);
    module_defun(fio_ptr, "f-dirname", &detail::Fdirname);
    module_defun(fio_ptr, "f-common-parent", &detail::Fcommon_parent);
    module_defun(fio_ptr, "f-ext", &detail::Fext);
    module_defun(fio_ptr, "f-no-ext", &detail::Fno_ext);
    module_defun(fio_ptr, "f-swap-ext", &detail::Fswap_ext);
    module_defun(fio_ptr, "f-base", &detail::Fbase);
    module_defun(fio_ptr, "f-relative", &detail::Frelative);
    module_defun(fio_ptr, "f-short", &detail::Fshort);
    module_defun(fio_ptr, "f-long", &detail::Flong);
    module_defun(fio_ptr, "f-canonical", &detail::Fcanonical);
    module_defun(fio_ptr, "f-full", &detail::Ffull);
    module_defun(fio_ptr, "f-exists", &detail::Fexists);
    module_defun(fio_ptr, "f-direcotry", &detail::Fdirecotry);
    module_defun(fio_ptr, "f-file", &detail::Ffile);
    module_defun(fio_ptr, "f-symlink", &detail::Fsymlink);
    module_defun(fio_ptr, "f-readable", &detail::Freadable);
    module_defun(fio_ptr, "f-writable", &detail::Fwritable);
    module_defun(fio_ptr, "f-executable", &detail::Fexecutable);
    module_defun(fio_ptr, "f-absolute", &detail::Fabsolute);
    module_defun(fio_ptr, "f-prelative", &detail::Fprelative);
    module_defun(fio_ptr, "f-is-root", &detail::Fis_root);
    module_defun(fio_ptr, "f-same", &detail::Fsame);
    module_defun(fio_ptr, "f-parent-of", &detail::Fparent_of);
    module_defun(fio_ptr, "f-child-of", &detail::Fchild_of);
    module_defun(fio_ptr, "f-ancestor-of", &detail::Fancestor_of);
    module_defun(fio_ptr, "f-descendant-of", &detail::Fdescendant_of);
    module_defun(fio_ptr, "f-hidden", &detail::Fhidden);
    module_defun(fio_ptr, "f-empty", &detail::Fempty);

    return Mfileio;
}


}  // namespace alisp
