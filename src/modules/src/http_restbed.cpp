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

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_asyncs.hpp"
#include "alisp/alisp/declarations/constants.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_eval.hpp"

#include "http/definitions.hpp"
#include "http/async_actions.hpp"
#include "http/language_space.hpp"


#include <memory>
#include <vector>
#include <unordered_map>

#include <restbed>


namespace http
{

using namespace alisp;

auto localhost = make_string("127.0.0.1");


ALObjectPtr Fend_request(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

    auto fut = AL_EVAL(t_obj, eval, 0);
    eval->async().submit_future(object_to_resource(fut->i(0)), Qt);
    return Qt;
}


}  // namespace http

ALISP_EXPORT alisp::env::ModulePtr init_http_restbed(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mhttp    = alisp::module_init("http-restbed");
    auto http_ptr = Mhttp.get();

    alisp::module_defvar(http_ptr, "localhost", http::localhost, R"()");

    // server config
    alisp::module_defun(http_ptr, "server", &http::Fserver, R"()");
    alisp::module_defun(http_ptr, "server-root", &http::Fserver_root, R"()");
    alisp::module_defun(http_ptr, "server-port", &http::Fserver_port, R"()");
    alisp::module_defun(http_ptr, "server-address", &http::Fserver_address, R"()");
    alisp::module_defun(http_ptr, "server-default-header", &http::Fserver_default_header, R"()");
    alisp::module_defun(http_ptr, "server-default-headers", &http::Fserver_default_headers, R"()");
    alisp::module_defun(http_ptr, "server-worker-limit", &http::Fserver_worker_limit, R"()");
    alisp::module_defun(http_ptr, "server-connection-limit", &http::Fserver_connection_limit, R"()");
    alisp::module_defun(http_ptr, "server-connection-timeout", &http::Fserver_connection_timeout, R"()");
    alisp::module_defun(http_ptr, "server-case-insensitive-uris", &http::Fserver_ci_uris, R"()");
    alisp::module_defun(http_ptr, "server-status-msg", &http::Fserver_status_msg, R"()");
    alisp::module_defun(http_ptr, "server-property", &http::Fserver_property, R"()");
    alisp::module_defun(http_ptr, "server-not-found-handler", &http::Fserver_not_found_handler, R"()");

    // server operations
    alisp::module_defun(http_ptr, "server-start", &http::Fserver_start, R"()");
    alisp::module_defun(http_ptr, "server-stop", &http::Fserver_stop, R"()");
    alisp::module_defun(http_ptr, "server-restart", &http::Fserver_restart, R"()");

    // route config
    alisp::module_defun(http_ptr, "route", &http::Froute, R"()");
    alisp::module_defun(http_ptr, "route-handler", &http::Froute_method_handler, R"()");
    alisp::module_defun(http_ptr, "route-path", &http::Froute_set_path, R"()");
    alisp::module_defun(http_ptr, "route-headers", &http::Froute_default_headers, R"()");
    alisp::module_defun(http_ptr, "route-header", &http::Froute_default_header, R"()");

    // request operations
    alisp::module_defun(http_ptr, "request-end", &http::Fend_request, R"()");


    alisp::module_eval(http_ptr, http::detail::language_definitons);


    return Mhttp;
}
