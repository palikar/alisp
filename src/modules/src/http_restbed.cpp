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


#include <restbed>


namespace http
{

using namespace alisp;

namespace detail
{

struct Server
{
    std::vector<restbed::Resource> g_resources;
    restbed::Settings g_settings;
    std::unique_ptr<restbed::Service> g_server;
    


};


inline management::Registry<uWS::App, 0x08> server_registry;

struct server_start
{

    static constexpr bool managed    = false;
    static constexpr bool has_future = false;

    ALObjectPtr g_id;

    server_start(ALObjectPtr id) : g_id(std::move(id)) {}

    ALObjectPtr operator()(async::AsyncS *async) const
    {
        
        auto t = std::thread([g_id = g_id, async = async]() {
            auto id = object_to_resource(g_id);
            auto &server = detail::server_registry[id];

            

            for (auto& resoruce : server.g_resoruces)
            {
                server.g_service->publish(res);
            }
            
            server.g_service->start(server.g_settings);
            async->async_reset_pending();
        });

        t.detach();
        return Qt;

    }
};


}  // namespace detail


ALObjectPtr Fserver(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto new_id = detail::server_registry.emplace_resource()->id;

    auto &server = detail::server_registry[new_id];
    server.g_service = std::make_unique<restbed::Service>();
    
    return resource_to_object(new_id);
}

ALObjectPtr Fserver_port(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto port  = AL_EVAL(t_obj, eval, 1);
        
    detail::server_registry[id].g_settings.set_port(port->to_int());
    
    return Qt;
}

ALObjectPtr Fserver_root(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto port  = AL_EVAL(t_obj, eval, 1);
        
    detail::server_registry[id].g_settings.set_root(port->to_string());
    
    return Qt;
}

ALObjectPtr Fserver_address(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto address  = AL_EVAL(t_obj, eval, 1);
        
    detail::server_registry[id].g_settings.set_address(address->to_string());
    
    return Qt;
}

ALObjectPtr Fserver_default_header(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval, 0));
    auto header  = AL_EVAL(t_obj, eval, 1);
    auto value  = AL_EVAL(t_obj, eval, 2);
        
    detail::server_registry[id].g_settings.set_default_header(header->to_string(), value->to_string());
    
    return Qt;
}

ALObjectPtr Fget(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval 0));
    auto route = AL_EVAL(t_obj, eval 1);
    auto fun   = AL_EVAL(t_obj, eval 2);

    auto res = restbed::Resource();
    resource->set_path(route->to_string());
    resource->set_method_handler( "GET", [](const shared_ptr< Session > session) {
        
    });

    detail::server_registry[id].g_resources.push_back(std::move(resource));

    return Qt;
}

ALObjectPtr Fpost(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(AL_EVAL(t_obj, eval 0));
    auto route = AL_EVAL(t_obj, eval 1);
    auto fun   = AL_EVAL(t_obj, eval 2);

    auto res = restbed::Resource();
    resource->set_path(route->to_string());
    resource->set_method_handler( "POST", [](const shared_ptr< Session > session) {
        
    });

    detail::server_registry[id].g_resources.push_back(std::move(resource));

    return Qt;
}

ALObjectPtr Fstart(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    eval->async().async_pending();
    return async::dispatch<detail::server_start>(eval->async(), eval->eval(t_obj->i(0)));


}

ALObjectPtr Fend_request(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

}


}  // namespace http

ALISP_EXPORT alisp::env::ModulePtr init_http_restbed(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mhttp    = alisp::module_init("http-restbed");
    auto http_ptr = Mhttp.get();

    alisp::module_defun(http_ptr, "server", &http::Fserver, R"()");
    alisp::module_defun(http_ptr, "get", &http::Fget, R"()");
    alisp::module_defun(http_ptr, "post", &http::Fpost, R"()");
    alisp::module_defun(http_ptr, "start", &http::Fstart, R"()");
    alisp::module_defun(http_ptr, "end-request", &http::Fend_request, R"()");


    return Mhttp;
}

