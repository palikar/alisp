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

#pragma once

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

namespace detail
{

struct Server
{
    std::unique_ptr<restbed::Service> g_server;
    std::shared_ptr<restbed::Settings> g_settings;
    std::vector<std::shared_ptr<restbed::Resource>> g_resources;
};

inline management::Registry<Server, 0x08> server_registry;

inline ALObjectPtr handle_request(const restbed::Request &request)
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

inline void handle_response(const restbed::Request &, restbed::Response &response, const ALObjectPtr &t_al_response)
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


extern ALObjectPtr Fserver(const ALObjectPtr &, env::Environment *, eval::Evaluator *);

extern ALObjectPtr Fserver_port(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_root(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_address(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_default_header(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_default_headers(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_worker_limit(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_connection_limit(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_ci_uris(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_connection_timeout(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_status_msg(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_property(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_not_found_handler(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_start(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_stop(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_restart(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);


extern ALObjectPtr Froute(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Froute_set_path(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Froute_default_headers(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Froute_default_header(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Froute_method_handler(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Froute_error_handler(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);


}  // namespace http
