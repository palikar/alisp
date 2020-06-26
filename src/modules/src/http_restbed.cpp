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

// inline management::Registry<uWS::App, 0x07> server_registry;

// struct server_start
// {

//     static constexpr bool managed    = false;
//     static constexpr bool has_future = false;

//     ALObjectPtr g_id;

//     server_start(ALObjectPtr id) : g_id(std::move(id)) {}

//     ALObjectPtr operator()(async::AsyncS *async) const
//     {

//     }
// };

}  // namespace detail


ALObjectPtr Fserver(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

}

ALObjectPtr Fget(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{



}

ALObjectPtr Fpost(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto id    = object_to_resource(eval->eval(t_obj->i(0)));
    auto route = eval->eval(t_obj->i(1));
    auto fun   = eval->eval(t_obj->i(2));

    

    return Qnil;
}

ALObjectPtr Fstart(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

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

