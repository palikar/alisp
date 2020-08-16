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
    using namespace http;

    auto Mhttp    = module_init("http-restbed");
    auto http_ptr = Mhttp.get();

    module_defvar(http_ptr, "localhost", localhost, R"()");

    module_defun(http_ptr, server::name, &server::func, server::doc, server::signature);
    module_defun(http_ptr,
                 server_static_root::name,
                 &server_static_root::func,
                 server_static_root::doc,
                 server_static_root::signature);
    module_defun(http_ptr,
                 server_static_route::name,
                 &server_static_route::func,
                 server_static_route::doc,
                 server_static_route::signature);
    module_defun(http_ptr,
                 server_templates_root::name,
                 &server_templates_root::func,
                 server_templates_root::doc,
                 server_templates_root::signature);
    module_defun(http_ptr, server_port::name, &server_port::func, server_port::doc, server_port::signature);
    module_defun(http_ptr, server_root::name, &server_root::func, server_root::doc, server_root::signature);
    module_defun(http_ptr, server_address::name, &server_address::func, server_address::doc, server_address::signature);
    module_defun(http_ptr,
                 server_default_header::name,
                 &server_default_header::func,
                 server_default_header::doc,
                 server_default_header::signature);
    module_defun(http_ptr,
                 server_default_headers::name,
                 &server_default_headers::func,
                 server_default_headers::doc,
                 server_default_headers::signature);
    module_defun(http_ptr,
                 server_worker_limit::name,
                 &server_worker_limit::func,
                 server_worker_limit::doc,
                 server_worker_limit::signature);
    module_defun(http_ptr,
                 server_connection_limit::name,
                 &server_connection_limit::func,
                 server_connection_limit::doc,
                 server_connection_limit::signature);
    module_defun(http_ptr, server_ci_uris::name, &server_ci_uris::func, server_ci_uris::doc, server_ci_uris::signature);
    module_defun(http_ptr,
                 server_connection_timeout::name,
                 &server_connection_timeout::func,
                 server_connection_timeout::doc,
                 server_connection_timeout::signature);
    module_defun(http_ptr,
                 server_status_msg::name,
                 &server_status_msg::func,
                 server_status_msg::doc,
                 server_status_msg::signature);
    module_defun(
      http_ptr, server_property::name, &server_property::func, server_property::doc, server_property::signature);
    module_defun(http_ptr,
                 server_not_found_handler::name,
                 &server_not_found_handler::func,
                 server_not_found_handler::doc,
                 server_not_found_handler::signature);
    module_defun(http_ptr, server_start::name, &server_start::func, server_start::doc, server_start::signature);
    module_defun(http_ptr, server_stop::name, &server_stop::func, server_stop::doc, server_stop::signature);
    module_defun(http_ptr, server_restart::name, &server_restart::func, server_restart::doc, server_restart::signature);
    module_defun(http_ptr, route::name, &route::func, route::doc, route::signature);
    module_defun(http_ptr, route_set_path::name, &route_set_path::func, route_set_path::doc, route_set_path::signature);
    module_defun(http_ptr,
                 route_default_headers::name,
                 &route_default_headers::func,
                 route_default_headers::doc,
                 route_default_headers::signature);
    module_defun(http_ptr,
                 route_default_header::name,
                 &route_default_header::func,
                 route_default_header::doc,
                 route_default_header::signature);
    module_defun(http_ptr,
                 route_method_handler::name,
                 &route_method_handler::func,
                 route_method_handler::doc,
                 route_method_handler::signature);
    module_defun(http_ptr, server::name, &server::func, server::doc, server::signature);

    module_eval(http_ptr, detail::language_definitons);

    return Mhttp;
}
