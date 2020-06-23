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

inline management::Registry<uWS::App, 0x07> server_registry;


}


ALObjectPtr Fserver(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto port   = eval->eval(t_obj->i(0));
    auto new_id = detail::server_registry.emplace_resource(us_socket_context_options_t{})->id;

    detail::server_registry[new_id].listen(port->to_int(), [](auto *listenSocket) {
        if (listenSocket)
        {
            std::cout << "Listening on port " << 9001 << std::endl;
        }
    });

    return resource_to_object(new_id);
}


ALObjectPtr Fget(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id = object_to_resource(eval->eval(t_obj->i(0)));
    auto route = eval->eval(t_obj->i(1));
    auto fun = eval->eval(t_obj->i(2));
    
    detail::server_registry[id].post(route->to_string(), [fun, eval](auto *res, auto *req) {

        // std::cout << req->getQuery() << "\n";
        // auto result = [&] {
        //     eval::detail::EvaluationLock lock{ *eval };
        //     return eval->handle_lambda(fun, make_list());
        // }();
        // res->writeHeader("Content-Type", "text/html; charset=utf-8")->end(dump(result));

        res->onAborted([](){
            std::cout << "aborted?" << "\n";

        });
        
        std::string buffer;
        res->onData([res, buffer = std::move(buffer)](std::string_view data, bool last) mutable {
            buffer.append(data.data(), data.length());
            if (last) {
                std::cout << buffer << std::endl;
                res->writeHeader("Content-Type", "text/html; charset=utf-8")->end(buffer);
            }
        });        

    });

    return Qnil;
}

struct server_start
{

    static constexpr bool managed    = false;
    static constexpr bool has_future = false;

    ALObjectPtr g_id;
    
    server_start(ALObjectPtr id) : g_id(std::move(id))
    {
    }

    ALObjectPtr operator()(async::AsyncS* async) const
    {

        
        auto t = std::thread([g_id = g_id, async=async](){
            auto id = object_to_resource(g_id);
            auto &l = detail::server_registry[id];
            l.run();
            async->async_reset_pending();
            
        });

        t.detach();
        return Qt;

    }
};


ALObjectPtr Fstart(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    // auto id = object_to_resource(eval->eval(t_obj->i(0)));
    // auto &l = detail::server_registry[id];
    // l.run();
    // return Qt;
    
    eval->async().async_pending();
    return async::dispatch<server_start>(eval->async(), eval->eval(t_obj->i(0)));
}


}  // namespace http

ALISP_EXPORT alisp::env::ModulePtr init_http(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mhttp    = alisp::module_init("http");
    auto http_ptr = Mhttp.get();

    alisp::module_defun(http_ptr, "server", &http::Fserver, R"()");
    alisp::module_defun(http_ptr, "get", &http::Fget, R"()");
    alisp::module_defun(http_ptr, "start", &http::Fstart, R"()");


    return Mhttp;
}


#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
