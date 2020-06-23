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

#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"


#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wuseless-cast"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wshadow"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wold-style-cast"
#endif

#include <uWebSockets/App.h>
#include <libusockets.h>

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif


#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

namespace http
{

using namespace alisp;

namespace detail
{

// inline management::Registry<std::unique_ptr<uWS::App>, 0x07> server_registry;


}


ALObjectPtr Fstart(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

    uWS::App()
        .get("/hello",
        [](auto *res, auto *) {
            res->writeHeader("Content-Type", "text/html; charset=utf-8")->end("Hello HTTP!");
        });

    //   .listen(9001, [](auto *listenSocket) {
    //       if (listenSocket)
    //       {
    //           std::cout << "Listening on port " << 9001 << std::endl;
    //       }
    //   });

    // .run();

    return Qnil;
}


}  // namespace http


ALISP_EXPORT alisp::env::ModulePtr init_http(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mhttp = alisp::module_init("http");
    auto http_ptr = Mhttp.get();

    alisp::module_defun(http_ptr, "start", &http::Fstart, R"()");


    return Mhttp;
}


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
