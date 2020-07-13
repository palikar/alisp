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


extern ALObjectPtr Fserver(const ALObjectPtr &, env::Environment *, eval::Evaluator *);

extern ALObjectPtr Fserver_static_root(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_static_route(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval);

extern ALObjectPtr Fserver_templates_root(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval);

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
