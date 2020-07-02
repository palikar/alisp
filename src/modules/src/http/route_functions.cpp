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


ALObjectPtr Froute(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj,  0);

    auto route = arg_eval(eval, obj,  1);

    auto res = std::make_shared<restbed::Resource>();
    res->set_path(route->to_string());

    auto &server = detail::server_registry[object_to_resource(id)];
    server.g_resources.push_back(res);

    return make_int(server.g_resources.size() - 1);
}

ALObjectPtr Froute_set_path(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj,  0);

    const auto route_id = arg_eval(eval, obj,  1);

    const auto route_path = arg_eval(eval, obj,  2);

    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_path(route_path->to_string());

    return Qt;
}

ALObjectPtr Froute_default_headers(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj,  0);

    const auto route_id = arg_eval(eval, obj,  1);

    const auto headers = arg_eval(eval, obj,  2);

    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    for (auto &header : *headers)
    {

        res->set_default_header(header->i(0)->to_string(), header->i(1)->to_string());
    }

    return Qt;
}

ALObjectPtr Froute_default_header(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj,  0);

    const auto route_id = arg_eval(eval, obj,  1);

    const auto header = arg_eval(eval, obj,  2);


    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_default_header(header->i(0)->to_string(), header->i(1)->to_string());

    return Qt;
}

ALObjectPtr Froute_method_handler(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj,  0);

    const auto route_id = arg_eval(eval, obj,  1);

    const auto method = arg_eval(eval, obj,  2);

    const auto handler_callback = arg_eval(eval, obj,  3);

    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_method_handler(
      method->to_string(), [eval, handler_callback](const std::shared_ptr<restbed::Session> session) {
          const auto request = session->get_request();

          const size_t content_length = request->get_header("Content-Length", size_t{ 0 });

          session->fetch(content_length,
                         [eval, handler_callback, request](const std::shared_ptr<restbed::Session> fetched_session,
                                                           const restbed::Bytes &) {
                             auto req_obj = detail::handle_request(*request.get());
                             auto res_obj = make_list();

                             auto future = eval->async().new_future(
                               [fetched_session, request, res_obj, req_obj](auto future_result) {
                                   if (is_falsy(future_result))
                                   {
                                       return;
                                   }

                                   restbed::Response response{};
                                   detail::handle_response(*request.get(), response, std::move(res_obj));
                                   fetched_session->close(response);
                               });

                             res_obj->children().push_back(resource_to_object(future));

                             eval->async().submit_callback(handler_callback, make_list(req_obj, res_obj));
                         });
      });

    return Qt;
}

ALObjectPtr Froute_error_handler(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj,  0);

    const auto route_id = arg_eval(eval, obj,  1);

    const auto handler_callback = arg_eval(eval, obj,  2);

    auto &server = detail::server_registry[object_to_resource(id)];
    auto &res    = server.g_resources[static_cast<size_t>(route_id->to_int())];

    res->set_error_handler([eval, handler_callback](const int code,
                                                    const std::exception &exc,
                                                    const std::shared_ptr<restbed::Session> session) {
        const auto request = session->get_request();

        const size_t content_length = request->get_header("Content-Length", size_t{ 0 });

        session->fetch(content_length,
                       [eval, handler_callback, request, code, exc](
                         const std::shared_ptr<restbed::Session> fetched_session, const restbed::Bytes &) {
                           auto req_obj = detail::handle_request(*request.get());
                           auto res_obj = make_list();

                           req_obj->children().push_back(make_string(":code"));
                           req_obj->children().push_back(make_int(code));
                           req_obj->children().push_back(make_string(":what"));
                           req_obj->children().push_back(make_string(exc.what()));

                           auto future =
                             eval->async().new_future([fetched_session, request, res_obj, req_obj](auto future_result) {
                                 if (is_falsy(future_result))
                                 {
                                     return;
                                 }

                                 restbed::Response response{};
                                 detail::handle_response(*request.get(), response, std::move(res_obj));
                                 fetched_session->close(response);
                             });

                           res_obj->children().push_back(resource_to_object(future));

                           eval->async().submit_callback(handler_callback, make_list(req_obj, res_obj));
                       });
    });

    return Qt;
}

}  // namespace http
