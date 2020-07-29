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
#include "http/response_handling.hpp"
#include "http/request_handling.hpp"
#include "http/language_space.hpp"


#include <memory>
#include <vector>
#include <unordered_map>

#include <restbed>

namespace http
{

using namespace alisp;


ALObjectPtr Froute(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    auto route = arg_eval(eval, obj, 1);

    auto res = std::make_shared<restbed::Resource>();
    res->set_path(route->to_string());

    auto &server = detail::server_registry[object_to_resource(id)];
    server.g_resources.push_back(res);

    return make_int(server.g_resources.size() - 1);
}

ALObjectPtr Froute_set_path(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto route_id = arg_eval(eval, obj, 1);

    const auto route_path = arg_eval(eval, obj, 2);

    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_path(route_path->to_string());

    return Qt;
}

ALObjectPtr Froute_default_headers(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto route_id = arg_eval(eval, obj, 1);

    const auto headers = arg_eval(eval, obj, 2);

    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    for (auto &header : *headers)
    {

        res->set_default_header(header->i(0)->to_string(), header->i(1)->to_string());
    }

    return Qt;
}

ALObjectPtr Froute_default_header(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto route_id = arg_eval(eval, obj, 1);

    const auto header = arg_eval(eval, obj, 2);


    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_default_header(header->i(0)->to_string(), header->i(1)->to_string());

    return Qt;
}

ALObjectPtr Froute_method_handler(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id               = arg_eval(eval, obj, 0);
    const auto route_id         = arg_eval(eval, obj, 1);
    const auto method           = arg_eval(eval, obj, 2);
    const auto handler_callback = arg_eval(eval, obj, 3);

    auto s_id    = object_to_resource(id);
    auto &server = detail::server_registry[s_id];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_method_handler(
      method->to_string(), [eval, handler_callback, s_id](const std::shared_ptr<restbed::Session> session) {
          const auto request          = session->get_request();
          const size_t content_length = request->get_header("Content-Length", size_t{ 0 });
          session->fetch(content_length,
                         [eval, handler_callback, request, s_id](
                           const std::shared_ptr<restbed::Session> fetched_session, const restbed::Bytes &) {
                             auto req_obj = detail::handle_request(*request.get());
                             detail::callback_response(handler_callback, req_obj, s_id, eval, fetched_session, request);
                         });
      });

    return Qt;
}

ALObjectPtr Froute_error_handler(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id               = arg_eval(eval, obj, 0);
    const auto route_id         = arg_eval(eval, obj, 1);
    const auto handler_callback = arg_eval(eval, obj, 2);

    auto s_id    = object_to_resource(id);
    auto &server = detail::server_registry[s_id];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_error_handler([eval, handler_callback, s_id](const int code,
                                                          const std::exception &exc,
                                                          const std::shared_ptr<restbed::Session> session) {
        const auto request          = session->get_request();
        const size_t content_length = request->get_header("Content-Length", size_t{ 0 });
        session->fetch(content_length,
                       [eval, handler_callback, request, code, exc, s_id](
                         const std::shared_ptr<restbed::Session> fetched_session, const restbed::Bytes &) {
                           auto req_obj = detail::handle_request(*request.get());

                           req_obj->children().push_back(make_string(":code"));
                           req_obj->children().push_back(make_int(code));
                           req_obj->children().push_back(make_string(":what"));
                           req_obj->children().push_back(make_string(exc.what()));

                           detail::callback_response(handler_callback, req_obj, s_id, eval, fetched_session, request);
                       });
    });

    return Qt;
}

}  // namespace http
