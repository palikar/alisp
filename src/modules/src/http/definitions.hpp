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

struct Server
{
    std::unique_ptr<restbed::Service> g_server;
    std::shared_ptr<restbed::Settings> g_settings;
    std::vector<std::shared_ptr<restbed::Resource>> g_resources;

    std::string static_root{ "" };
    std::string templates_root{ "" };

    std::unordered_map<std::string, std::pair<std::string, bool>> mimes{
        { ".txt", { "text/plain", false } },
        { ".html", { "text/html", false } },
        { ".htm", { "text/html", false } },
        { ".css", { "text/css", false } },
        { ".jpeg", { "image/jpg", true } },
        { ".jpg", { "image/jpg", true } },
        { ".png", { "image/png", true } },
        { ".gif", { "image/gif", true } },
        { ".svg", { "image/svg+xml", false } },
        { ".ico", { "image/x-icon", true } },
        { ".json", { "application/json", false } },
        { ".pdf", { "application/pdf", true } },
        { ".js", { "application/javascript", false } },
        { ".wasm", { "application/wasm", true } },
        { ".xml", { "application/xml", false } },
        { ".xhtml", { "application/xhtml+xml", false } },
    };

    ALObjectPtr not_found_handler;

    std::unordered_map<std::string, inja::Template> templates;
    inja::Environment template_env;


    inline void setup_template_env()
    {
        namespace fs = std::filesystem;

        if (!fs::is_directory(templates_root))
        {
            return;
        }

        for (auto entry : fs::recursive_directory_iterator(templates_root))
        {
            if (!fs::is_regular_file(entry))
            {
                continue;
            }

            auto file  = entry.path().string();
            auto name  = utility::trim(utility::replace_all(file, fs::path(templates_root), ""), '/');
            auto &temp = templates.insert({ name, template_env.parse_template(file) }).first->second;
            template_env.include_template(name, temp);
            template_env.set_trim_blocks(true);
            template_env.set_lstrip_blocks(true);
        }
    }
};

inline management::Registry<Server, 0x08> server_registry;


}  // namespace detail

struct server
{
    inline static const std::string name{ "server" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &, env::Environment *, eval::Evaluator *);
    
};

struct server_static_root
{

    inline static const std::string name{ "server-static-root" };

    inline static const Signature signature{Int{}, String{}};

    inline static const std::string doc{ R"()" };

    ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval);

};

struct server_static_route
{
    inline static const std::string name{ "server-static-route" };

    inline static const Signature signature{Int{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval);

};

struct server_templates_root
{
    inline static const std::string name{ "server-templates-root" };

    inline static const Signature signature{Int{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval);

};

struct server_port
{
    inline static const std::string name{ "server-port" };

    inline static const Signature signature{Int{}, Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_root
{
    inline static const std::string name{ "server-root" };

    inline static const Signature signature{Int{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_address
{
    inline static const std::string name{ "server-address" };

    inline static const Signature signature{Int{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_default_header
{
    inline static const std::string name{ "server-default-header" };

    inline static const Signature signature{Int{}, String{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_default_headers
{
    inline static const std::string name{ "server-default-headers" };

    inline static const Signature signature{Int{}, List{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_worker_limit
{
    inline static const std::string name{ "server-worker-limit" };

    inline static const Signature signature{Int{}, Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_connection_limit
{
    inline static const std::string name{ "server-connection-limit" };

    inline static const Signature signature{Int{}, Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_ci_uris
{
    inline static const std::string name{ "server-ci-uris" };

    inline static const Signature signature{Int{}, Any{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_connection_timeout
{
    inline static const std::string name{ "server-connection-timeout" };

    inline static const Signature signature{Int{}, Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_status_msg
{
    inline static const std::string name{ "server-status-msg" };

    inline static const Signature signature{Int{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_property
{
    inline static const std::string name{ "server-property" };

    inline static const Signature signature{Int{}, String{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_not_found_handler
{
    inline static const std::string name{ "server-not-found-handler" };

    inline static const Signature signature{Int{}, Function{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_start
{
    inline static const std::string name{ "server-start" };

    inline static const Signature signature{Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_stop
{
    inline static const std::string name{ "server-stop" };

    inline static const Signature signature{Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct server_restart
{
    inline static const std::string name{ "server-restart" };

    inline static const Signature signature{Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};


struct route
{
    inline static const std::string name{ "route" };

    inline static const Signature signature{Int{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct route_set_path
{
    inline static const std::string name{ "route-set-path" };

    inline static const Signature signature{Int{}, Int{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct route_default_headers
{
    inline static const std::string name{ "route-default-headers" };

    inline static const Signature signature{Int{}, Int{}, List{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct route_default_header
{
    inline static const std::string name{ "route-default-header" };

    inline static const Signature signature{Int{}, Int{}, String{}, String{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct route_method_handler
{
    inline static const std::string name{ "route-method-handler" };

    inline static const Signature signature{Int{}, Int{}, Function{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};

struct route_error_handler
{
    inline static const std::string name{ "route-error-handler" };

    inline static const Signature signature{Int{}, Int{}, Function{}};

    inline static const std::string doc{ R"()" };
    
    ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval);

};



}  // namespace http
