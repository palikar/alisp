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
#include "alisp/utility.hpp"

#include "./json_render.hpp"
#include "./util.hpp"
#include "./definitions.hpp"

#include <memory>
#include <vector>
#include <unordered_map>
#include <stdio.h>
#include <time.h>

#include <fmt/format.h>
#include <restbed>
#include <inja.hpp>


namespace http
{

using namespace alisp;

namespace detail
{


inline void send_response(uint32_t, restbed::Response &response, const std::shared_ptr<const restbed::Request> request, const std::shared_ptr<restbed::Session> session)
{

    if (request->get_header("Connection", "close").compare("keep-alive") == 0)
    {
        response.set_header("Connection", "keep-alive");
        // session->yield(response);
        session->close(response);
    }
    else
    {
        response.set_header("Connection", "close");
        session->close(response);
    }
}

inline void default_headers(restbed::Response &response)
{
    response.set_header("Date", gat_date());
}

inline void attach_file(Server &server, const std::filesystem::path &path, restbed::Response &response)
{
    bool binary = false;
    // deduce mime type
    if (path.has_extension())
    {
        auto ext = path.extension();
        if (server.mimes.count(ext) > 0)
        {
            std::string content_type;
            std::tie(content_type, binary) = server.mimes.at(ext);
            response.set_header("Content-Type", std::move(content_type));
        }
    }

    // use some cache thing here

    if (!binary)
    {
        std::string content = utility::load_file(path);
        response.set_header("Content-Length", std::to_string(std::size(content)));
        response.set_body(std::move(content));
    }
    else
    {
        std::vector<unsigned char> content = utility::load_file_binary(path);
        response.set_header("Content-Length", std::to_string(std::size(content)));
        response.set_body(std::move(content));
    }
    response.set_status_code(200);
}

inline void handle_cookie(Server &, restbed::Response &response, const ALObjectPtr &t_cookie)
{
    std::string cookie_expr = fmt::format("{}=\"{}\"", t_cookie->i(0)->to_string(), t_cookie->i(1)->to_string());
    // expires

    // max age
    if (auto [age, succ] = get_next(t_cookie, ":lifetime-s"); succ)
    {
        cookie_expr += fmt::format("; Max-Age={}", age->to_int());
    }

    // domain
    if (auto [domain, succ] = get_next(t_cookie, ":domain"); succ)
    {
        cookie_expr += fmt::format("; Domain={}", domain->to_string());
    }

    // path
    if (auto [path, succ] = get_next(t_cookie, ":path"); succ)
    {
        cookie_expr += fmt::format("; Path={}", path->to_string());
    }

    // secure
    if (contains(t_cookie, ":https-only"))
    {
        cookie_expr += "; Secure";
    }

    // HttpOnly
    if (contains(t_cookie, ":https-only"))
    {
        cookie_expr += "; HttpOnly";
    }

    response.set_header("Set-Cookie", cookie_expr);
}

inline void handle_file(Server &server, restbed::Response &response, const ALObjectPtr &t_file)
{
    namespace fs = std::filesystem;

    fs::path path{ t_file->i(0)->to_string() };

    if (path.is_relative())
    {
        if (contains(t_file, ":static"))
        {
            path = server.static_root / path;
        }
        else
        {
            path = server.templates_root / path;
        }
    }

    if (!(fs::exists(path) and fs::is_regular_file(path)))
    {
        return;
    }

    attach_file(server, path, response);
}

inline void render_file(Server &server, restbed::Response &response, const ALObjectPtr &t_params)
{
    namespace fs = std::filesystem;
    std::string template_name{ t_params->i(0)->to_string() };

    auto json = json_render(t_params->i(1));

    auto result = [&] {

        if (server.templates.count(template_name) > 0)
        {
            return server.template_env.render(server.templates.at(template_name), json);
        }

        if (auto p = fs::path{ template_name }; p.is_absolute() and fs::is_regular_file(p))
        {
            return server.template_env.render_file(fs::absolute(p), json);
        }

        return std::string{ "" };
    }();

    response.set_status_code(200);
    response.set_body(result);
    response.set_header("Content-Length", std::to_string(std::size(result)));
    response.set_header("Content-Type", "text/html");
}

inline void handle_response(uint32_t s_id, restbed::Response &response, const ALObjectPtr &t_al_response)
{
    auto &server = server_registry[s_id];
    AL_DEBUG("Handling response:"s += dump(t_al_response));

    for (size_t i = 1; i < std::size(*t_al_response); ++i)
    {


        // Lower level response handling
        
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
            handle_cookie(server, response, t_al_response->i(i + 1));
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":content") and pstring(t_al_response->i(i + 1)))
        {
            auto content = t_al_response->i(i + 1)->to_string();
            response.set_header("Content-Length", std::to_string(std::size(content)));
            response.set_body(std::move(content));
            ++i;
        } 


        // Higher level response handling
        if (sym_name(t_al_response->i(i), ":file") and plist(t_al_response->i(i + 1)))
        {
            handle_file(server, response, t_al_response->i(i + 1));
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":render") and plist(t_al_response->i(i + 1)))
        {
            render_file(server, response, t_al_response->i(i + 1));
            ++i;
        }

        if (sym_name(t_al_response->i(i), ":redirect") and pstring(t_al_response->i(i + 1)))
        {
            response.set_header("Location", fmt::format("{}", t_al_response->i(i + 1)->to_string()));
            response.set_status_code(301);
            ++i;
        }

        
    }

    default_headers(response);
    response.set_header("Cache-Control", "no-store");
}

inline void callback_response(ALObjectPtr callback, ALObjectPtr req_obj, uint32_t s_id, eval::Evaluator *eval, const std::shared_ptr<restbed::Session> session, const std::shared_ptr<const restbed::Request> request)
{

    auto res_obj = make_list();
    auto future  = eval->async().new_future([session, res_obj, req_obj, s_id, request](auto future_result) {

        if (is_falsy(future_result))
        {
            restbed::Response response{};
            session->close(response);
            return;
        }
        
        restbed::Response response{};
        detail::handle_response(s_id, response, std::move(res_obj));

        send_response(s_id, response, request, session);
    });

    res_obj->children().push_back(resource_to_object(future));
    eval->async().submit_callback(callback, make_list(req_obj, res_obj));
}


}

}
