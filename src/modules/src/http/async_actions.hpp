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

#include "./definitions.hpp"

namespace http
{

using namespace alisp;

namespace detail
{

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

struct server_stop
{

    static constexpr bool managed    = false;
    static constexpr bool has_future = false;

    ALObjectPtr g_id;

    server_stop(ALObjectPtr id) : g_id(std::move(id)) {}

    ALObjectPtr operator()(async::AsyncS *async) const
    {

        auto thread = std::thread([g_id = g_id, async = async]() {
            auto id      = object_to_resource(g_id);
            auto &server = detail::server_registry[id];


            for (auto &resource : server.g_resources)
            {
                server.g_server->publish(resource);
            }

            server.g_server->stop();
            async->async_reset_pending();
        });

        thread.detach();
        return Qt;
    }
};

struct server_restart
{

    static constexpr bool managed    = false;
    static constexpr bool has_future = false;

    ALObjectPtr g_id;

    server_restart(ALObjectPtr id) : g_id(std::move(id)) {}

    ALObjectPtr operator()(async::AsyncS *async) const
    {

        auto thread = std::thread([g_id = g_id, async = async]() {
            auto id      = object_to_resource(g_id);
            auto &server = detail::server_registry[id];


            for (auto &resource : server.g_resources)
            {
                server.g_server->publish(resource);
            }

            server.g_server->restart();
            async->async_reset_pending();
        });

        thread.detach();
        return Qt;
    }
};

}  // namespace detail


}  // namespace http
