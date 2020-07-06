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


ALObjectPtr Fend_request(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{

    auto fut = arg_eval(eval, obj, 0);
    eval->async().submit_future(object_to_resource(fut->i(0)), Qt);
    return Qt;
}


}  // namespace http

ALISP_EXPORT alisp::env::ModulePtr init_http_restbed(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mhttp    = module_init("http-restbed");
    auto http_ptr = Mhttp.get();

    alisp::module_defvar(http_ptr, "localhost", http::localhost, R"()");

    // server config
    module_defun(http_ptr, "server", &http::Fserver, R"()");

    module_defun(http_ptr, "server-root", &http::Fserver_root, R"()");
    module_defun(http_ptr, "server-port", &http::Fserver_port, R"()");
    module_defun(http_ptr, "server-static-root", &http::Fserver_static_root, R"()");
    module_defun(http_ptr, "server-static-route", &http::Fserver_static_route, R"()");
    module_defun(http_ptr, "server-templates-root", &http::Fserver_templates_root, R"()");

    module_defun(http_ptr, "server-address", &http::Fserver_address, R"()");
    module_defun(http_ptr, "server-default-header", &http::Fserver_default_header, R"()");
    module_defun(http_ptr, "server-default-headers", &http::Fserver_default_headers, R"()");
    module_defun(http_ptr, "server-worker-limit", &http::Fserver_worker_limit, R"()");
    module_defun(http_ptr, "server-connection-limit", &http::Fserver_connection_limit, R"()");
    module_defun(http_ptr, "server-connection-timeout", &http::Fserver_connection_timeout, R"()");
    module_defun(http_ptr, "server-case-insensitive-uris", &http::Fserver_ci_uris, R"()");
    module_defun(http_ptr, "server-status-msg", &http::Fserver_status_msg, R"()");
    module_defun(http_ptr, "server-property", &http::Fserver_property, R"()");
    module_defun(http_ptr, "server-not-found-handler", &http::Fserver_not_found_handler, R"()");

    // server operations
    module_defun(http_ptr, "server-start", &http::Fserver_start, R"()");
    module_defun(http_ptr, "server-stop", &http::Fserver_stop, R"()");
    module_defun(http_ptr, "server-restart", &http::Fserver_restart, R"()");

    // route config
    module_defun(http_ptr, "route", &http::Froute, R"()");
    module_defun(http_ptr, "route-handler", &http::Froute_method_handler, R"()");
    module_defun(http_ptr, "route-path", &http::Froute_set_path, R"()");
    module_defun(http_ptr, "route-headers", &http::Froute_default_headers, R"()");
    module_defun(http_ptr, "route-header", &http::Froute_default_header, R"()");

    // request operations
    module_defun(http_ptr, "request-end", &http::Fend_request, R"()");

    // language space
    module_eval(http_ptr, http::detail::language_definitons);


    // module_signature(http_ptr, "server-root", Signature(Int{}, String{}));
    // module_signature(http_ptr, "server-port", Signature(Int{}, Int{}));
    // module_signature(http_ptr, "server-address", Signature(Int{}, String{}));
    // module_signature(http_ptr, "server-default-header", Signature(Int{}, String{}, String{}));
    // module_signature(http_ptr, "server-default-headers", Signature(Int{}, List{}));
    // module_signature(http_ptr, "server-worker-limit", Signature(Int{}, Int{}));
    // module_signature(http_ptr, "server-connection-limit", Signature(Int{}, Int{}));
    // module_signature(http_ptr, "server-connection-timeout", Signature(Int{}, Int{}));
    // module_signature(http_ptr, "server-case-insensitive-uris", Signature(Int{}, Sym{}));
    // module_signature(http_ptr, "server-status-msg", Signature(Int{}, Int{}, String{}));
    // module_signature(http_ptr, "server-property", Signature(Int{}, String{}, String{}));
    // module_signature(http_ptr, "server-not-found-handler", Signature(Int{}, Function{}));
    // module_signature(http_ptr, "server-start", Signature(Int{}));
    // module_signature(http_ptr, "server-stop", Signature(Int{}));
    // module_signature(http_ptr, "server-restart", Signature(Int{}));

    // module_signature(http_ptr, "route", Signature(Int{}, String{}));
    // module_signature(http_ptr, "route-handler", Signature(Int{}, Int{}, Function{}));
    // module_signature(http_ptr, "route-path", Signature(Int{}, Int{}, String{}));
    // module_signature(http_ptr, "route-headers", Signature(Int{}, Int{}, List{}));
    // module_signature(http_ptr, "route-header", Signature(Int{}, Int{}, String{}, String{}));

    return Mhttp;
}
