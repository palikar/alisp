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


#include <memory>
#include <vector>
#include <unordered_map>

#include <restbed>


namespace http
{

using namespace alisp;

auto localhost = make_string("127.0.0.1");

namespace detail
{

struct Server
{
    std::unique_ptr<restbed::Service> g_server;
    std::shared_ptr<restbed::Settings> g_settings;
    std::vector<std::shared_ptr<restbed::Resource>> g_resources;
};

inline management::Registry<Server, 0x08> server_registry;

struct server_start
{

    static constexpr bool managed    = false;
    static constexpr bool has_future = false;

    ALObjectPtr g_id;

    server_start(ALObjectPtr id) : g_id(std::move(id)) {}

    ALObjectPtr operator()(async::AsyncS *async) const
    {

        auto thread = std::thread([g_id = g_id, async = async]() {
            auto id      = object_to_resource(g_id);
            auto &server = detail::server_registry[id];


            for (auto &resource : server.g_resources)
            {
                server.g_server->publish(resource);
            }

            server.g_server->start(server.g_settings);
            async->async_reset_pending();
        });

        thread.detach();
        return Qt;
    }
};

bool sym_name(const ALObjectPtr &t_obj, const std::string &t_name)
{
    return psym(t_obj) and t_obj->to_string().compare(t_name) == 0;
}

ALObjectPtr handle_request(const restbed::Request &request)
{
    ALObject::list_type list;

    list.push_back(make_symbol(":body"));
    list.push_back(make_string(std::string{ request.get_body().begin(), request.get_body().end() }));

    list.push_back(make_symbol(":method"));
    list.push_back(make_string(request.get_method()));  // 2

    list.push_back(make_symbol(":host"));
    list.push_back(make_string(request.get_host()));  // 5

    list.push_back(make_symbol(":path"));
    list.push_back(make_string(request.get_path()));  // 7

    list.push_back(make_symbol(":protocol"));
    list.push_back(make_string(request.get_protocol()));  // 9

    list.push_back(make_symbol(":version"));
    list.push_back(make_double(request.get_version()));  // 11

    ALObject::list_type header_list;
    for (const auto &[name, value] : request.get_headers())
    {
        header_list.push_back(make_object(name, value));
    }

    list.push_back(make_symbol(":headers"));
    list.push_back(make_list(header_list));  // 13

    ALObject::list_type path_params_list;
    for (const auto &[name, value] : request.get_path_parameters())
    {
        path_params_list.push_back(make_object(name, value));
    }

    list.push_back(make_symbol(":path-parameters"));
    list.push_back(make_list(path_params_list));  // 15

    ALObject::list_type query_params_list;
    for (const auto &[name, value] : request.get_query_parameters())
    {
        query_params_list.push_back(make_object(name, value));
    }

    list.push_back(make_symbol(":query-parameters"));
    list.push_back(make_list(query_params_list));  // 17

    list.push_back(make_symbol(":port"));
    list.push_back(make_int(request.get_port()));  // 19

    return make_list(list);
}

void handle_response(const restbed::Request &, restbed::Response &response, const ALObjectPtr &t_al_response)
{
    for (size_t i = 1; i < std::size(*t_al_response); ++i)
    {

        if (sym_name(t_al_response->i(i), ":content") and pstring(t_al_response->i(i + 1)))
        {

            response.set_body(t_al_response->i(i + 1)->to_string());
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":code") and pint(t_al_response->i(i + 1)))
        {
            response.set_status_code(static_cast<int>(t_al_response->i(i + 1)->to_int()));
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":header") and plist(t_al_response->i(i + 1)))
        {
            response.set_header(t_al_response->i(i + 1)->i(0)->to_string(), t_al_response->i(i + 1)->i(1)->to_string());
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":cookie") and plist(t_al_response->i(i + 1)))
        {
            // handle cookie please!
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":file") and plist(t_al_response->i(i + 1)))
        {
            // handle file please!
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":render") and plist(t_al_response->i(i + 1)))
        {
            // handle rendering file please!
            ++i;
        }
    }
}

}  // namespace detail


ALObjectPtr Fserver(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
{
    auto new_id = detail::server_registry.emplace_resource()->id;

    auto &server = detail::server_registry[new_id];

    server.g_settings = std::make_unique<restbed::Settings>();
    server.g_server   = std::make_unique<restbed::Service>();

    return resource_to_object(new_id);
}

ALObjectPtr Fserver_port(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id   = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto port = AL_EVAL(t_obj, eval, 1);

    detail::server_registry[id].g_settings->set_port(static_cast<uint16_t>(port->to_int()));

    return Qt;
}

ALObjectPtr Fserver_root(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id   = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto port = AL_EVAL(t_obj, eval, 1);

    detail::server_registry[id].g_settings->set_root(port->to_string());

    return Qt;
}

ALObjectPtr Fserver_address(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id      = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto address = AL_EVAL(t_obj, eval, 1);

    detail::server_registry[id].g_settings->set_bind_address(address->to_string());

    return Qt;
}

ALObjectPtr Fserver_default_header(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id     = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto header = AL_EVAL(t_obj, eval, 1);
    auto value  = AL_EVAL(t_obj, eval, 2);

    detail::server_registry[id].g_settings->set_default_header(header->to_string(), value->to_string());

    return Qt;
}

ALObjectPtr Froute(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto route = AL_EVAL(t_obj, eval, 1);

    auto res = std::make_shared<restbed::Resource>();
    res->set_path(route->to_string());

    auto &server = detail::server_registry[id];
    server.g_resources.push_back(res);

    return make_int(server.g_resources.size() - 1);
}

ALObjectPtr Fhandler(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    const auto id               = object_to_resource(AL_EVAL(t_obj, eval, 0));
    const auto route_id         = AL_EVAL(t_obj, eval, 1);
    const auto method           = AL_EVAL(t_obj, eval, 2);
    const auto handler_callback = AL_EVAL(t_obj, eval, 3);

    auto &server = detail::server_registry[id];
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

ALObjectPtr Fstart(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    eval->async().async_pending();
    return async::dispatch<detail::server_start>(eval->async(), eval->eval(t_obj->i(0)));
}

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

    alisp::module_defun(http_ptr, "server", &http::Fserver, R"()");
    alisp::module_defun(http_ptr, "set-root", &http::Fserver_root, R"()");
    alisp::module_defun(http_ptr, "set-port", &http::Fserver_port, R"()");
    alisp::module_defun(http_ptr, "set-address", &http::Fserver_address, R"()");
    alisp::module_defun(http_ptr, "set-default-header", &http::Fserver_default_header, R"()");
    alisp::module_defun(http_ptr, "start", &http::Fstart, R"()");

    alisp::module_defun(http_ptr, "route", &http::Froute, R"()");
    alisp::module_defun(http_ptr, "route-handler", &http::Fhandler, R"()");
    alisp::module_defun(http_ptr, "route-path", &http::Fhandler, R"()");
    alisp::module_defun(http_ptr, "route-header", &http::Fhandler, R"()");

    alisp::module_defun(http_ptr, "end-request", &http::Fend_request, R"()");

    alisp::module_eval(http_ptr, R"(

(import 'nargs :all)



;; Low level routing

(defun http--route-get (serv rout callback)
  (route-handler serv rout "GET" callback))

(defun http--route-post (serv rout callback)
  (route-handler serv rout "POST" callback))

(defun http--route-head (serv rout callback)
  (route-handler serv rout "HEAD" callback))

(defun http--route-delete (serv rout callback)
  (route-handler serv rout "DELETE" callback))

(defun http--route-patch (serv rout callback)
  (route-handler serv rout "PATCH" callback))



;; user friendly routing 

(defun route-post (serv index callback)
  ([serv] http--route-post index
   (lambda (req res)
     (callback req res (lambda () (end-request res))))))


(defun route-get (serv index callback)
  ([serv] http--route-get index
   (lambda (req res)
     (callback req res (lambda () (end-request res))))))





;; Response handling

(defun set-content (res cont)
  (push res :content)
  (push res cont))

(defun set-status-code (res code)
  (push res :code)
  (push res code))


(defun set-header (res name value)
  (push res :header)
  (push res `(,name ,value)))


(defun set-cookie (res name value)
  (push res :cookie)
  (push res `(,name ,value)))




;; Request handling

(defun body (req)
  ([req] nth 1))

(defun method (req)
  ([req] nth 3))

(defun host (req)
  ([req] nth 5))

(defun path (req)
  ([req] nth 7))

(defun protocol (req)
  ([req] nth 9))

(defun version (req)
  ([req] nth 11))

(defun headers (req)
  ([req] nth 13))

(defun path-parameters (req)
  ([req] nth 15))

(defun query-parameters (req)
  ([req] nth 17))

(defun port (req)
  ([req] nth 19))


;; Headers

(defun has-header (req name)
  (dolist (header (headers req))
    (when (equal name ([header] nth 0))
      (return t)))
  (return nil))

(defun header (req name)
  (dolist (header (headers req))
    (when (equal name ([header] nth 0))
      (return ([header] nth 1))))
  (return nil))



;; Query parameters

(defun has-query-parameter (req name)
  (dolist (parameter (query-parameters req))
    (when (equal name ([parameter] nth 0))
      (return t)))
  (return nil))

(defun query-parameter (req name)
  (dolist (parameter (query-parameters req))
    (when (equal name ([parameter] nth 0))
      (return ([parameter] nth 1))))
  (return nil))




;; Path parameters

(defun has-path-parameter (req name)
  (dolist (parameter (path-parameters req))
    (when (equal name ([parameter] nth 0))
      (return t)))
  (return nil))

(defun path-parameter (req name)
  (dolist (parameter (path-parameters req))
    (when (equal name ([parameter] nth 0))
      (return ([parameter] nth 1))))
  (return nil))



)");


    return Mhttp;
}
