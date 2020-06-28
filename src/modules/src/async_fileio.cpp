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
#include "alisp/alisp/alisp_asyncs.hpp"
#include "alisp/alisp/alisp_eval.hpp"

#include "alisp/alisp/async/timing.hpp"
#include "alisp/alisp/async/action.hpp"
#include "alisp/config.hpp"
#include "alisp/utility.hpp"

#include <filesystem>

namespace async_fileio
{

using namespace alisp;

namespace detail
{

struct read_file_text
{
    static constexpr bool managed    = true;
    static constexpr bool has_future = false;

    std::string g_file;
    ALObjectPtr callback;

    read_file_text(std::string t_file, ALObjectPtr t_callback)
      : g_file(std::move(t_file)), callback(std::move(t_callback))

    {
    }

    ALObjectPtr operator()(async::AsyncS *async) const
    {
        namespace fs = std::filesystem;

        if (!fs::exists(g_file))
        {
            async->submit_callback(callback, make_list(Qnil));
        }

        auto content = utility::load_file(g_file);
        async->submit_callback(callback, make_list(make_string(content)));

        return Qt;
    }
};

struct write_file_text
{
    static constexpr bool managed    = true;
    static constexpr bool has_future = false;

    std::string g_file;
    std::string g_content;
    bool g_append;
    ALObjectPtr callback;

    write_file_text(std::string t_file, std::string t_content, ALObjectPtr t_callback, bool t_append = false)
      : g_file(std::move(t_file)), g_content(std::move(t_content)), g_append(t_append), callback(std::move(t_callback))

    {
    }

    ALObjectPtr operator()(async::AsyncS *async) const
    {
        namespace fs = std::filesystem;

        if (!fs::exists(g_file))
        {
            async->submit_callback(callback, make_list(Qnil));
        }

        if (!fs::is_regular_file(g_file))
        {
            async->submit_callback(callback, make_list(Qnil));
        }

        utility::dump_file(g_file, g_content, g_append);

        async->submit_callback(callback, make_list(Qt));

        return Qt;
    }
};

}  // namespace detail

ALObjectPtr Fasync_append_text(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);

    auto file_name    = AL_EVAL(obj, eval, 0);
    auto file_content = AL_EVAL(obj, eval, 1);
    auto callback     = AL_EVAL(obj, eval, 2);

    AL_CHECK(assert_string(file_name));
    AL_CHECK(assert_string(file_content));
    AL_CHECK(assert_function(callback));

    return async::dispatch<detail::write_file_text>(
      eval->async(), file_name->to_string(), file_content->to_string(), std::move(callback), true);
}

ALObjectPtr Fasync_write_text(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);

    auto file_name    = AL_EVAL(obj, eval, 0);
    auto file_content = AL_EVAL(obj, eval, 1);
    auto callback     = AL_EVAL(obj, eval, 2);

    AL_CHECK(assert_string(file_name));
    AL_CHECK(assert_string(file_content));
    AL_CHECK(assert_function(callback));

    return async::dispatch<detail::write_file_text>(
      eval->async(), file_name->to_string(), file_content->to_string(), std::move(callback), false);
}

ALObjectPtr Fasync_read_text(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);

    auto file_name = AL_EVAL(obj, eval, 0);
    auto callback  = AL_EVAL(obj, eval, 1);

    AL_CHECK(assert_string(file_name));
    AL_CHECK(assert_function(callback));

    auto file = file_name->to_string();

    return async::dispatch<detail::read_file_text>(eval->async(), file, std::move(callback));
}

}  // namespace async_fileio

ALISP_EXPORT alisp::env::ModulePtr init_async_fileio(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;
    auto M       = alisp::module_init("async-fileio");
    auto aio_ptr = M.get();


    module_defun(aio_ptr, "async-write-text", async_fileio::Fasync_write_text, R"()");
    module_defun(aio_ptr, "async-append-text", async_fileio::Fasync_append_text, R"()");
    module_defun(aio_ptr, "async-read-text", async_fileio::Fasync_read_text, R"()");

    return M;
}
