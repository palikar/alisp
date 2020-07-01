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
#include <fmt/format.h>

namespace alisp
{

auto fileio_signal = alisp::make_symbol("fileio-signal");

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
    for (size_t i = 0; i < glob_result.gl_pathc; ++i)
    {
        filenames.push_back(string(glob_result.gl_pathv[i]));
    }

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
        if (home or ((home = getenv("USERPROFILE"))))
        {
            path.replace(0, 1, home);
        }
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


struct root
{
    inline static const std::string name{"f-root"};
    
    inline static const std::string doc{R"((f-root)

Return absolute root.
)"};

    
    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
    {
        namespace fs = std::filesystem;
        AL_CHECK(assert_size<0>(t_obj));

        try
        {
            return make_string(fs::current_path().root_path());
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
        return Qnil;
    }

};

struct directories
{
    inline static const std::string name{"f-directories"};

    inline static const std::string doc{R"((f-directories PATH)

Find all directories in `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        ALObject::list_type entries;

        try
        {
            for (auto &entr : fs::directory_iterator(path->to_string()))
            {
                if (!entr.is_directory())
                {
                    continue;
                }
                entries.push_back(make_string(entr.path().string()));
            }
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }

        return make_object(entries);
    }

};

struct entries
{

    inline static const std::string name{"f-entries"};

    inline static const std::string doc{R"((f-entries PATH)

Find all files and directories in `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        ALObject::list_type entries;

        try
        {
            for (auto &entr : fs::directory_iterator(path->to_string()))
            {
                entries.push_back(make_string(entr.path().string()));
            }
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }

        return make_object(entries);
    }

};

struct glob
{

    inline static const std::string name{"f-glob"};

    inline static const std::string doc{R"((f-glob PATTERN PATH)

Find `PATTERN` in `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

};

struct touch
{

    inline static const std::string name{"f-touch"};

    inline static const std::string doc{R"((f-touch PATH)

Update `PATH` last modification date or create if it does not exist.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        std::fstream fs;
        fs.open(path->to_string(), std::ios::out | std::ios::app);
        if (!fs.is_open())
        {
            return Qnil;
        }
        fs.close();

        return Qt;
    }

};

struct expand_user
{
    inline static const std::string name{"f-expand-user"};

    inline static const std::string doc{R"((f-expand-user PATH)

For unix systems, expand `~` to the location of the home directory of
the current user.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        return make_string(expand_user(path->to_string()));
    }

};

struct copy
{

    inline static const std::string name{"f-copy"};

    inline static const std::string doc{R"((f-copy FROM TO)

Copy file or directory `FROM` to `TO`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }

        return Qt;
    }

};

struct move
{

    inline static const std::string name{"f-move"};

    inline static const std::string doc{R"((f-move FROM TO)

Move or rename `FROM` to `TO`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }


        return Qt;
    }

};

struct make_symlink
{

    inline static const std::string name{"f-make-symlink"};

    
    inline static const std::string doc{R"((f-make-symlink SOURCE PATH)

Create a symlink to `SOURCE` from `PATH`.
)"};


    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }


        return Qt;
    }

};

struct Sdelete
{

    inline static const std::string name{"f-delete"};

    inline static const std::string doc{R"((f-delete PATH)

Delete `PATH`, which can be file or directory.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            if (std::size(*t_obj) > 1 and is_truthy(eval->eval(t_obj->i(1))))
            {

                bool val = fs::remove_all(path->to_string());
                return val ? Qt : Qnil;
            }
            else
            {
                bool val = fs::remove(path->to_string());
                return val ? Qt : Qnil;
            }
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct mkdir
{
    
    
    inline static const std::string name{"f-mkdir"};

    inline static const std::string doc{R"((f-mkdir DIR)

Create the directory `DIR`.
)"};
    
    
    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            bool val = fs::create_directory(path->to_string());
            return val ? Qt : Qnil;
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct with_temp_file
{

    inline static const std::string name{"f-with-temp-file"};

    inline static const std::string doc{R"((f-temp-file PATH)

Return a resource object ot a temporary file. The file is created and
the object can be used for writing to the file.
)"};

    inline static const std::string doc{R"((f-with-temp-file FILE-SYM BODY)

Bind `FILE-SYM` and execute the forms in `BODY`. `FILE-SYM` will point
to a valid file resource of a temporary file.
)"};
    

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
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

};

struct temp_file_name
{

    inline static const std::string name{"f-temp-file-name"};

    inline static const std::string name{R"((f-temp-file-name PATH)

Return a path to a temporary file. The file is not created but the
path will be valid for a temporary file.
)"};
    
    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<0>(t_obj));
        return make_string(FileHelpers::temp_file_path());
    }

};

struct temp_file
{
    inline static const std::string name{"f-temp-file"};

    inline static const std::string doc{R"((f-temp-file PATH)

Return a resource object ot a temporary file. The file is created and
the object can be used for writing to the file.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<0>(t_obj));
        auto path = FileHelpers::temp_file_path();
        return FileHelpers::put_file(path, std::fstream(path, std::ios::out), false, true);
    }

};

struct read_bytes
{

    inline static const std::string name{"f-read-bytes"};

    inline static const std::string doc{R"((f-read-bytes PATH)

Read binary data from `PATH`. Return the binary data as byte array.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));

    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    if (!fs::exists(path->to_string()))
    {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string()))
    {
        return Qnil;
    }

    std::ifstream infile(path->to_string().c_str(), std::ios::in | std::ios::ate | std::ios::binary);
    if (!infile.is_open())
    {
        return Qnil;
    }

    auto size = infile.tellg();
    infile.seekg(0, std::ios::beg);
    assert(size >= 0);

    std::vector<char> v(static_cast<size_t>(size));
    infile.read(&v[0], static_cast<std::streamsize>(size));

    ALObject::list_type bytes;
    for (auto &ch : v)
    {
        bytes.push_back(make_int(static_cast<int>(ch)));
    }

    return make_object(bytes);
}

};

struct read_text
{

    inline static const std::string name{"f-read-text"};
    inline static const std::string doc{R"((f-read-text PATH)

Read the text from the file `PATH` and return the contatns as a string.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));


    if (!fs::exists(path->to_string()))
    {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string()))
    {
        return Qnil;
    }

    return make_string(utility::load_file(path->to_string()));
}

};

struct write_text
{

    inline static const std::string name{"f-write-text"};

    inline static const std::string doc{R"((f-write-text PATH TEXT)

Write `TEXT` to the file pointed by `PATH`. Previous content is erased.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<2>(t_obj));

        auto path = eval->eval(t_obj->i(0));
        auto text = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(path));
        AL_CHECK(assert_string(text));


        if (!fs::exists(path->to_string()))
        {
            return Qnil;
        }
        if (!fs::is_regular_file(path->to_string()))
        {
            return Qnil;
        }

        std::ofstream outfile;
        outfile.open(path->to_string(), std::ios_base::out);
        outfile << text->to_string();
        outfile.close();

        return Qt;
    }

};

struct write_bytes
{

    inline static const std::string name{"f-write-bytes"};

    inline static const std::string doc{R"((f-write-bytes PATH BYTES)

Write the bytes `BYTES` to the file pointed by `PATH`. Previous content is erased.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<2>(t_obj));

        auto path  = eval->eval(t_obj->i(0));
        auto bytes = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(path));
        AL_CHECK(assert_byte_array(bytes));

        if (!fs::exists(path->to_string()))
        {
            return Qnil;
        }
        if (!fs::is_regular_file(path->to_string()))
        {
            return Qnil;
        }

        std::ofstream outfile;
        outfile.open(path->to_string(), std::ios_base::out | std::ios_base::binary);
        if (outfile.is_open())
        {
            return Qnil;
        }
        for (auto &b : *bytes)
        {
            outfile.put(static_cast<char>(b->to_int()));
        }
        outfile.close();

        return Qt;
    }

};

struct append_text
{

    inline static const std::string name{"f-append-text"};

    inline static const std::string doc{R"((f-append-text PATH TEXT)

Append `TEXT` to the file pointed by `PATH`. This function does not
erase the prevous contents of the file.  )"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<2>(t_obj));

        auto path = eval->eval(t_obj->i(0));
        auto text = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(path));
        AL_CHECK(assert_string(text));


        if (!fs::exists(path->to_string()))
        {
            return Qnil;
        }
        if (!fs::is_regular_file(path->to_string()))
        {
            return Qnil;
        }

        std::ofstream outfile;
        outfile.open(path->to_string(), std::ios_base::app);
        outfile << text->to_string();
        outfile.close();

        return Qt;
    }

};

struct append_bytes
{

    inline static const std::string name{"f-append-bytes"};

    inline static const std::string doc{R"((f-append-bytes PATH BYTES)

Append the bytes `BYTES` to the file pointed by `PATH`. This function does not
erase the prevous contents of the file.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));

    auto path  = eval->eval(t_obj->i(0));
    auto bytes = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_byte_array(bytes));

    if (!fs::exists(path->to_string()))
    {
        return Qnil;
    }
    if (!fs::is_regular_file(path->to_string()))
    {
        return Qnil;
    }

    std::ofstream outfile;
    outfile.open(path->to_string(), std::ios_base::out | std::ios_base::binary | std::ios_base::app);
    if (outfile.is_open())
    {
        return Qnil;
    }
    for (auto &b : *bytes)
    {
        outfile.put(static_cast<char>(b->to_int()));
    }
    outfile.close();

    return Qt;
}

};

struct join
{

    inline static const std::string name{"f-join"};

    inline static const std::string doc{R"((f-join [ARGS] ...)

Join `ARGS` to a single path.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

};

struct split
{

    inline static const std::string name{"f-split"};

    inline static const std::string doc{R"((f-split PATH)

Split `PATH` and return list containing parts.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

    auto parts = utility::split(path->to_string(), fs::path::preferred_separator);

    return make_list(parts);
}

};

struct expand
{

    inline static const std::string name{"f-expand"};

    inline static const std::string doc{R"((f-expand PATH DIR)

Expand `PATH` relative to `DIR`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            const auto p = fs::absolute(path->to_string());
            return make_string(p);
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct filename
{

    inline static const std::string name{"f-filename"};

    inline static const std::string doc{R"((f-filename PATH)

Return the name of `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));


        try
        {
            const auto p = fs::path(path->to_string());
            return make_string(p.filename());
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct dirname
{

    inline static const std::string name{"f-dirname"};

    inline static const std::string doc{R"((f-dirname PATH)

Return the parent directory to `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));


        try
        {
            const auto p = fs::path(path->to_string());
            return make_string(p.parent_path());
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct common_parent
{
    inline static const std::string name{"f-common-parent"};

    inline static const std::string doc{R"((f-common-parent [PATHS] ...)

Return the deepest common parent directory of `PATHS`.
)"};
    
    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        return Qnil;
    }

};

struct ext
{

    inline static const std::string name{"f-ext"};

inline static const std::string doc{R"((f-ext PATH)

)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));

    try
    {
        const auto p = fs::path(path->to_string());
        return make_string(p.extension());
    }
    catch (fs::filesystem_error &exc)
    {
        signal(
          fileio_signal,
          fmt::format(
            "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
        return Qnil;
    }
}

};

struct no_ext
{

    inline static const std::string name{"f-no-ext"};

    inline static const std::string doc{R"((f-no-ext PATH)

)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            const auto p = fs::path(path->to_string());
            return make_string(p.stem());
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct swap_ext
{
    inline static const std::string name{"f-swap-ext"};

    inline static const std::string doc{R"((f-swap-ext PATH)

Return the file extension of `PATH`. The extension, in a file name, is
the part that follows the last ’.’, excluding version numbers and
backup suffixes.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<2>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        auto ext  = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(path));
        AL_CHECK(assert_string(ext));

        try
        {
            const auto p = fs::path(path->to_string());
            return make_string(p.stem().string() + "." + ext->to_string());
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct base
{

    inline static const std::string name{"f-base"};

    inline static const std::string doc{R"((f-base PATH)

Return the name of `PATH`, excluding the extension of file.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    auto ext  = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path));
    AL_CHECK(assert_string(ext));

    const auto p = fs::path(path->to_string());
    if (fs::is_directory(p))
    {
        return Qnil;
    }

    return make_string(p.stem().filename());
}

};

struct relative
{

    inline static const std::string name{"f-relative"};

    inline static const std::string doc{R"((f-relative PATH)

)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_min_size<2>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        const auto p = fs::path(path->to_string());

        try
        {
            if (std::size(*t_obj) > 1)
            {
                auto to_path = eval->eval(t_obj->i(1));
                AL_CHECK(assert_string(to_path));
                return make_string(fs::relative(p, to_path->to_string()));
            }
            return make_string(path->to_string());
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct short
{
    inline static const std::string name{"f-short"};

    inline static const std::string doc{R"((f-short PATH)

Return abbrev of `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        return Qnil;
    }

};

struct long
{

    inline static const std::string name{"f-long"};

    inline static const std::string doc{R"((f-long PATH)

Return long version of `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            return make_string(fs::canonical(path->to_string()));
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct canonical
{
    inline static const std::string name{"f-cannonical"};

    inline static const std::string doc{R"((f-canonical PATH)

Return the canonical name of `PATH`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            return make_string(fs::canonical(path->to_string()));
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct full
{

    inline static const std::string name{"f-full"};

    inline static const std::string doc{R"((f-full PATH)

Return absolute path to `PATH`, with ending slash.
)"};
    
    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            auto p = fs::path(path->to_string());
            return make_string(fs::absolute(p));
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct exists
{

    inline static const std::string name{"f-exists"};

    inline static const std::string doc{R"((f-exists PATH)

Return `t` if `PATH` exists, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        const auto p = fs::path(path->to_string());
        return fs::exists(p) ? Qt : Qnil;
    }

};

struct direcotry
{
    inline static const std::string name{"f-directory"};

inline static const std::string doc{R"((f-direcotry PATH)

Return `t` if `PATH` is directory, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::is_directory(p) ? Qt : Qnil;
}

};

struct file
{

    inline static const std::string name{"f-file"};

    inline static const std::string doc{R"((f-file PATH)

Return `t` if `PATH` is `nil`, false otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        auto p = fs::path(path->to_string());
        return fs::is_regular_file(p) ? Qt : Qnil;
    }

};

struct symlink
{

    inline static const std::string name{"f-symlink"};

    inline static const std::string doc{R"((f-symlink PATH)

Return `t` if `PATH` is symlink, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        auto p = fs::path(path->to_string());
        return fs::is_symlink(p) ? Qt : Qnil;
    }

};

struct readable
{

    inline static const std::string name{"f-readable"};

inline static const std::string doc{R"((f-readable PATH)

Return `t` if `PATH` is readable, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());

    return (fs::status(p).permissions() & fs::perms::owner_read) != fs::perms::none ? Qt : Qnil;
}

};

struct writable
{

    inline static const std::string name{"f-writable"};

    inline static const std::string doc{R"((f-writable PATH)

Return `t` if `PATH` is writable, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        auto p = fs::path(path->to_string());

        return (fs::status(p).permissions() & fs::perms::owner_write) != fs::perms::none ? Qt : Qnil;
    }

};

struct executable
{

    inline static const std::string name{"f-executable"};

    inline static const std::string doc{R"((f-executable PATH)

Return `t` if `PATH` is executable, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        auto p = fs::path(path->to_string());

        return (fs::status(p).permissions() & fs::perms::owner_exec) != fs::perms::none ? Qt : Qnil;
    }

};

struct absolute
{

    inline static const std::string name{"f-absolute"};

    inline static const std::string doc{R"((f-absolute PATH)

Return `t` if `PATH` is absolute, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        auto p = fs::path(path->to_string());
        return p.is_absolute() ? Qt : Qnil;
    }

};

struct prelative
{
    inline static const std::string name{"f-prelative"};

    inline static const std::string doc{R"((f-prelative PATH)

Return `t` if `PATH` is relative, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        auto p = fs::path(path->to_string());
        return p.is_relative() ? Qt : Qnil;
    }

};

struct is_root
{

    inline static const std::string name{"f-is-root"};

inline static const std::string doc{R"((f-is-root PATH)

Return `t` if `PATH` is root directory, `nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<1>(t_obj));
    auto path = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(path));
    auto p = fs::path(path->to_string());
    return fs::equivalent(p, fs::current_path().root_path()) ? Qt : Qnil;
}

};

struct same
{

    inline static const std::string name{"f-same"};

inline static const std::string doc{R"((f-same PATH1 PATH2)

Return `t` if `PATH1` and `PATH2` are references to same file.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = fs::path(path1->to_string());
    const auto p2 = fs::path(path2->to_string());

    try
    {
        return fs::equivalent(p1, p2) ? Qt : Qnil;
    }
    catch (fs::filesystem_error &exc)
    {
        signal(
          fileio_signal,
          fmt::format(
            "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
        return Qnil;
    }
}

};

struct parent_of
{
    inline static const std::string name{"f-parent-of"};

    inline static const std::string doc{R"((f-parent-of PATH1 PATH2)

Return t if `PATH1` is parent of `PATH2`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    AL_CHECK(assert_size<2>(t_obj));
    auto path1 = eval->eval(t_obj->i(0));
    auto path2 = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(path1));
    AL_CHECK(assert_string(path2));
    const auto p1 = fs::path(path1->to_string());
    const auto p2 = fs::path(path2->to_string());

    try
    {
        return fs::equivalent(p1, p2.parent_path()) ? Qt : Qnil;
    }
    catch (fs::filesystem_error &exc)
    {
        signal(
          fileio_signal,
          fmt::format(
            "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
        return Qnil;
    }
}

};

struct child_of
{
    inline static const std::string name{"f-child-of"};

    inline static const std::string doc{R"((f-child-of PATH1 PATH2)

Return t if `PATH1` is child of `PATH2`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        AL_CHECK(assert_size<2>(t_obj));
        auto path1 = eval->eval(t_obj->i(0));
        auto path2 = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(path1));
        AL_CHECK(assert_string(path2));
        const auto p1 = fs::path(path1->to_string());
        const auto p2 = fs::path(path2->to_string());

        try
        {
            return fs::equivalent(p1, p2.parent_path()) ? Qt : Qnil;
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct ancestor_of
{
    inline static const std::string name{"f-anscestor-of"};

    inline static const std::string doc{R"((f-ancestor-of PATH1 PATH2)

Return `t` if `PATH1` is ancestor of `PATH2`.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

            if (std::size(parts2) <= i)
            {
                return Qnil;
            }

            if (parts2[i] != parts1[i])
            {
                return Qnil;
            }
        }

        return Qt;
    }

};

struct descendant_of
{
    inline static const std::string name{"f-descendant-of"};

    inline static const std::string doc{R"((f-descendant-of PATH)

Return `t` if `PATH1` is desendant of `PATH2`.
)"};
    

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
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

        if (std::size(parts2) <= i)
        {
            return Qnil;
        }

        if (parts2[i] != parts1[i])
        {
            return Qnil;
        }
    }

    return Qt;
}

};

struct hidden
{

    inline static const std::string name{"f-hidden"};

    inline static const std::string doc{R"((f-hidden PATH)

Return `t` if `PATH` is hidden, `nil` otherwise.
)"};
    
    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    
    {
        namespace fs = std::filesystem;
        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));
        const auto p = fs::path(path->to_string());
        return p.filename().string()[0] == '.' ? Qt : Qnil;
    }

};

struct empty
{
    inline static const std::string name{"f-empty"};

    inline static const std::string doc{R"((f-empty PATH)

If `PATH` is a file, return `t` if the file in `PATH` is empty, `nil`
otherwise. If `PATH` is directory, return `t` if directory has no files,
`nil` otherwise.
)"};

    static     ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;
        AL_CHECK(assert_size<1>(t_obj));
        auto path = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(path));

        try
        {
            const auto p = fs::path(path->to_string());
            return fs::is_empty(p) ? Qt : Qnil;
        }
        catch (fs::filesystem_error &exc)
        {
            signal(
                fileio_signal,
                fmt::format(
                    "Fileio error: {}\nInvolved path(s): {} , {}", exc.what(), exc.path1().string(), exc.path2().string()));
            return Qnil;
        }
    }

};

struct module_doc
{

    inline static const std::string doc{R"(The `fileio` moudule provides utilities for working with file paths,
files, directories and some basic IO functions.
)"};

};




}  // namespace detail

env::ModulePtr init_fileio(env::Environment *, eval::Evaluator *)
{

    auto Mfileio = module_init("fileio");
    auto fio_ptr = Mfileio.get();

    module_doc(fio_ptr, detail::module_doc::doc);

    

    return Mfileio;
}


}  // namespace alisp
