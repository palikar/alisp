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

ALObjectPtr Fserver(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
{
    auto new_id = detail::server_registry.emplace_resource()->id;

    auto &server = detail::server_registry[new_id];

    server.g_settings = std::make_unique<restbed::Settings>();
    server.g_server   = std::make_unique<restbed::Service>();

    return resource_to_object(new_id);
}

ALObjectPtr Fserver_port(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    auto port = arg_eval(eval, obj, 1);

    detail::server_registry[object_to_resource(id)].g_settings->set_port(static_cast<uint16_t>(port->to_int()));

    return Qt;
}

ALObjectPtr Fserver_root(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    auto root = arg_eval(eval, obj, 1);

    detail::server_registry[object_to_resource(id)].g_settings->set_root(root->to_string());

    return Qt;
}

ALObjectPtr Fserver_address(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    auto address = arg_eval(eval, obj, 1);

    auto address_string = address->to_string();
    if (address_string.compare("localhost") == 0)
    {
        address_string = "127.0.0.1";
    }

    detail::server_registry[object_to_resource(id)].g_settings->set_bind_address(address_string);

    return Qt;
}

ALObjectPtr Fserver_default_header(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    auto header = arg_eval(eval, obj, 1);

    auto value = arg_eval(eval, obj, 2);

    detail::server_registry[object_to_resource(id)].g_settings->set_default_header(header->to_string(),
                                                                                   value->to_string());

    return Qt;
}

ALObjectPtr Fserver_default_headers(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto headers = arg_eval(eval, obj, 1);

    auto &server = detail::server_registry[object_to_resource(id)];

    for (auto &header : *headers)
    {

        server.g_settings->set_default_header(header->i(0)->to_string(), header->i(1)->to_string());
    }

    return Qt;
}

ALObjectPtr Fserver_worker_limit(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto limit = arg_eval(eval, obj, 1);

    auto &server = detail::server_registry[object_to_resource(id)];

    server.g_settings->set_worker_limit(static_cast<unsigned int>(limit->to_int()));

    return Qt;
}

ALObjectPtr Fserver_connection_limit(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = (arg_eval(eval, obj, 0));

    const auto limit = arg_eval(eval, obj, 1);

    auto &server = detail::server_registry[object_to_resource(id)];

    server.g_settings->set_connection_limit(static_cast<unsigned int>(limit->to_int()));

    return Qt;
}

ALObjectPtr Fserver_ci_uris(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto value = arg_eval(eval, obj, 1);

    auto &server = detail::server_registry[object_to_resource(id)];

    server.g_settings->set_case_insensitive_uris(is_truthy(value));

    return Qt;
}

ALObjectPtr Fserver_connection_timeout(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto value = arg_eval(eval, obj, 1);

    auto &server = detail::server_registry[object_to_resource(id)];

    server.g_settings->set_connection_timeout(std::chrono::seconds{ value->to_int() });

    return Qt;
}

ALObjectPtr Fserver_status_msg(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto status = arg_eval(eval, obj, 1);

    const auto msg = arg_eval(eval, obj, 2);

    auto &server = detail::server_registry[object_to_resource(id)];

    server.g_settings->set_status_message(static_cast<int>(status->to_int()), msg->to_string());

    return Qt;
}

ALObjectPtr Fserver_property(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto name = arg_eval(eval, obj, 1);

    const auto value = arg_eval(eval, obj, 2);

    auto &server = detail::server_registry[object_to_resource(id)];

    server.g_settings->set_property(name->to_string(), value->to_string());

    return Qt;
}

ALObjectPtr Fserver_not_found_handler(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id = arg_eval(eval, obj, 0);

    const auto handler_callback = arg_eval(eval, obj, 1);

    auto &server = detail::server_registry[object_to_resource(id)];


    server.g_server->set_not_found_handler([eval, handler_callback](const std::shared_ptr<restbed::Session> session) {
        const auto request = session->get_request();

        const size_t content_length = request->get_header("Content-Length", size_t{ 0 });

        session->fetch(content_length,
                       [eval, handler_callback, request](const std::shared_ptr<restbed::Session> fetched_session,
                                                         const restbed::Bytes &) {
                           auto req_obj = detail::handle_request(*request.get());
                           auto res_obj = make_list();

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

ALObjectPtr Fserver_start(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    eval->async().async_pending();
    return async::dispatch<detail::server_start>(eval->async(), id);
}

ALObjectPtr Fserver_stop(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    eval->async().async_pending();
    return async::dispatch<detail::server_stop>(eval->async(), id);
}

ALObjectPtr Fserver_restart(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = arg_eval(eval, obj, 0);

    eval->async().async_pending();
    return async::dispatch<detail::server_restart>(eval->async(), id);
}

}  // namespace http
