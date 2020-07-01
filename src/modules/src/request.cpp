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


#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshadow"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wnon-virtual-dtor"
// #pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include <cpr/cpr.h>

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/utility/files.hpp"
#include "alisp/utility/string_utils.hpp"

namespace request
{
using namespace alisp;

auto type_get     = alisp::make_symbol("GET");
auto type_post    = alisp::make_symbol("POST");
auto type_head    = alisp::make_symbol("HEAD");
auto type_delete  = alisp::make_symbol("DELETE");
auto type_patch   = alisp::make_symbol("PATCH");
auto type_put     = alisp::make_symbol("PUT");
auto type_options = alisp::make_symbol("OPTIONS");

namespace detail
{

static constexpr size_t GET_TYPE     = 0;
static constexpr size_t POST_TYPE    = 1;
static constexpr size_t HEAD_TYPE    = 2;
static constexpr size_t DELETE_TYPE  = 3;
static constexpr size_t PATCH_TYPE   = 4;
static constexpr size_t PUT_TYPE     = 5;
static constexpr size_t OPTIONS_TYPE = 6;


auto do_send(cpr::Session &session, size_t type)
{

    switch (type)
    {
        case GET_TYPE: return session.Get();
        case POST_TYPE: return session.Post();
        case HEAD_TYPE: return session.Head();
        case DELETE_TYPE: return session.Delete();
        case PATCH_TYPE: return session.Patch();
        case PUT_TYPE: return session.Put();
        case OPTIONS_TYPE: return session.Options();
        default: return session.Get();
    }
}

auto handle_headers(cpr::Header t_header)
{
    ALObject::list_type ret_list;

    for (auto &[name, value] : t_header)
    {
        ret_list.push_back(make_object(name, value));
    }

    return make_list(ret_list);
}

auto handle_cookies(cpr::Cookies t_cookies)
{
    ALObject::list_type ret_list;

    for (auto &[name, value] : t_cookies)
    {
        ret_list.push_back(make_object(name, value));
    }

    return make_list(ret_list);
}

auto send(cpr::Session &session, size_t type)
{
    auto resp = do_send(session, type);

    auto ret = make_list(make_string(resp.text),
                         make_int(resp.status_code),
                         handle_headers(resp.header),
                         make_string(resp.url),
                         make_real(resp.elapsed),
                         handle_cookies(resp.cookies));

    return ret;
}

}  // namespace detail


struct request
{
    inline static const std::string name{ "request" };

    inline static const Signature signature{ String{}, List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_min_size<1>(t_obj));

        auto url = eval->eval(t_obj->i(0));
        AL_CHECK(assert_string(url));

        cpr::Session session;
        cpr::priv::set_option(session, cpr::Url{ url->to_string() });

        size_t request_type{ 0 };

        if (std::size(*t_obj) > 1)
        {

            auto op = eval->eval(t_obj->i(1));
            AL_CHECK(assert_list(op));

            if (auto [type, succ] = get_next(op, ":type"); succ)
            {

                auto s = eval->eval(type);

                if (eq(s, type_get))
                {
                    request_type = detail::GET_TYPE;
                }
                else if (eq(s, type_post))
                {
                    request_type = detail::POST_TYPE;
                }
                else if (eq(s, type_delete))
                {
                    request_type = detail::DELETE_TYPE;
                }
                else if (eq(s, type_head))
                {
                    request_type = detail::HEAD_TYPE;
                }
                else if (eq(s, type_patch))
                {
                    request_type = detail::PATCH_TYPE;
                }
                else
                {
                    request_type = detail::GET_TYPE;
                }
            }

            if (auto [data, succ] = get_next(op, ":data"); succ)
            {
                auto s = eval->eval(data);
                AL_CHECK(assert_string(s));
                auto dat = s->to_string();
                cpr::priv::set_option(session, cpr::Body{ dat });
            }

            if (auto [params, succ] = get_next(op, ":params"); succ)
            {
                auto s = eval->eval(params);
                cpr::Parameters pars;
                cpr::CurlHolder holder;
                AL_CHECK(assert_list(s));
                for (auto &par : *s)
                {
                    auto key   = par->i(0);
                    auto value = par->i(1);
                    pars.AddParameter(cpr::Parameter(key->to_string(), value->to_string()), holder);
                }
                cpr::priv::set_option(session, pars);
            }

            if (auto [headers, succ] = get_next(op, ":headers"); succ)
            {
                auto s = eval->eval(headers);
                cpr::Header head;
                AL_CHECK(assert_list(s));
                for (auto &par : *s)
                {
                    auto key   = par->i(0);
                    auto value = par->i(1);
                    head.insert({ key->to_string(), value->to_string() });
                }
                cpr::priv::set_option(session, head);
            }

            if (auto [params, succ] = get_next(op, ":payload"); succ)
            {
                auto s = eval->eval(params);
                cpr::Payload pay{};
                cpr::CurlHolder holder;
                AL_CHECK(assert_list(s));
                for (auto &par : *s)
                {
                    auto key   = par->i(0);
                    auto value = par->i(1);
                    pay.AddPair({ key->to_string(), value->to_string() }, holder);
                }
                cpr::priv::set_option(session, pay);
            }

            if (auto [params, succ] = get_next(op, ":files"); succ)
            {
                auto s = eval->eval(params);
                cpr::Multipart multi{};
                AL_CHECK(assert_list(s));
                for (auto &par : *s)
                {
                    auto name = par->i(0);
                    auto file = par->i(1);
                    multi.parts.push_back({ name->to_string(), cpr::File(file->to_string()) });
                }
                cpr::priv::set_option(session, multi);
            }

            if (auto [params, succ] = get_next(op, ":timeout"); succ)
            {
                auto s = eval->eval(params);
                AL_CHECK(assert_int(s));
                cpr::priv::set_option(session, cpr::Timeout(static_cast<std::int32_t>(s->to_int())));
            }

            if (auto [params, succ] = get_next(op, ":auth"); succ)
            {
                auto s = eval->eval(params);
                AL_CHECK(assert_list(s));
                auto name = eval->eval(s->i(0));
                auto pass = eval->eval(s->i(1));
                cpr::priv::set_option(session, cpr::Authentication{ name->to_string(), pass->to_string() });
            }

            if (auto [params, succ] = get_next(op, ":digest"); succ)
            {
                auto s = eval->eval(params);
                AL_CHECK(assert_list(s));
                auto name = eval->eval(s->i(0));
                auto pass = eval->eval(s->i(1));
                cpr::priv::set_option(session, cpr::Digest{ name->to_string(), pass->to_string() });
            }

            if (auto [params, succ] = get_next(op, ":NTLM"); succ)
            {
                auto s = eval->eval(params);
                AL_CHECK(assert_list(s));
                auto name = eval->eval(s->i(0));
                auto pass = eval->eval(s->i(1));
                cpr::priv::set_option(session, cpr::NTLM{ name->to_string(), pass->to_string() });
            }

            if (auto [params, succ] = get_next(op, ":cookies"); succ)
            {
                auto s = eval->eval(params);
                cpr::Cookies cok{};
                cpr::CurlHolder holder;
                AL_CHECK(assert_list(s));
                for (auto &par : *s)
                {
                    auto key              = par->i(0);
                    auto value            = par->i(1);
                    cok[key->to_string()] = value->to_string();
                }
                cpr::priv::set_option(session, cok);
            }
        }

        return detail::send(session, request_type);
    }
};

struct body
{
    inline static const std::string name{ "body" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto resp_list = eval->eval(t_obj->i(0));
        if (resp_list->size() == 6)
        {
            return resp_list->i(0);
        }
        return Qnil;
    }
};

struct status_code
{
    inline static const std::string name{ "status-code" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto resp_list = eval->eval(t_obj->i(0));
        if (resp_list->size() == 6)
        {
            return resp_list->i(1);
        }
        return Qnil;
    }
};

struct headers
{
    inline static const std::string name{ "headers" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto resp_list = eval->eval(t_obj->i(0));
        if (resp_list->size() == 6)
        {
            return resp_list->i(2);
        }
        return Qnil;
    }
};

struct url
{
    inline static const std::string name{ "url" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto resp_list = eval->eval(t_obj->i(0));
        if (resp_list->size() == 6)
        {
            return resp_list->i(3);
        }
        return Qnil;
    }
};

struct elapsed
{
    inline static const std::string name{ "elapsed" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto resp_list = eval->eval(t_obj->i(0));
        if (resp_list->size() == 6)
        {
            return resp_list->i(4);
        }
        return Qnil;
    }
};

struct cookies
{
    inline static const std::string name{ "cookies" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"()" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto resp_list = eval->eval(t_obj->i(0));
        if (resp_list->size() == 6)
        {
            return resp_list->i(5);
        }
        return Qnil;
    }
};


struct get_var
{
    inline static const std::string name = "GET";

    inline static const std::string doc{ R"()" };

    inline static const auto var = type_get;
};

struct post_var
{
    inline static const std::string name = "POST";

    inline static const std::string doc{ R"()" };

    inline static const auto var = type_post;
};

struct head_var
{
    inline static const std::string name = "HEAD";

    inline static const std::string doc{ R"()" };

    inline static const auto var = type_head;
};

struct put_var
{
    inline static const std::string name = "PUT";

    inline static const std::string doc{ R"()" };

    inline static const auto var = type_put;
};

struct delete_var
{
    inline static const std::string name = "DELETE";

    inline static const std::string doc{ R"()" };

    inline static const auto var = type_delete;
};

struct options_var
{
    inline static const std::string name = "OPTIONS";

    inline static const std::string doc{ R"()" };

    inline static const auto var = type_options;
};

struct module_doc
{
    inline static const std::string doc{ R"()" };
};

}  // namespace request

ALISP_EXPORT alisp::env::ModulePtr init_request(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mrequest = alisp::module_init("request");
    auto req_ptr  = Mrequest.get();

    module_doc(req_ptr, request::module_doc::doc);

    module_defvar(req_ptr, request::get_var::name, request::get_var::var, request::get_var::doc);
    module_defvar(req_ptr, request::post_var::name, request::post_var::var, request::post_var::doc);
    module_defvar(req_ptr, request::head_var::name, request::head_var::var, request::head_var::doc);
    module_defvar(req_ptr, request::put_var::name, request::put_var::var, request::put_var::doc);
    module_defvar(req_ptr, request::delete_var::name, request::delete_var::var, request::delete_var::doc);
    module_defvar(req_ptr, request::options_var::name, request::options_var::var, request::options_var::doc);
    
    module_defun(req_ptr, request::request::name, request::request::func, request::request::doc, request::request::signature.al());
    module_defun(req_ptr, request::body::name, request::body::func, request::body::doc, request::body::signature.al());
    module_defun(req_ptr, request::status_code::name, request::status_code::func, request::status_code::doc, request::status_code::signature.al());
    module_defun(req_ptr, request::headers::name, request::headers::func, request::headers::doc, request::headers::signature.al());
    module_defun(req_ptr, request::url::name, request::url::func, request::url::doc, request::url::signature.al());
    module_defun(req_ptr, request::elapsed::name, request::elapsed::func, request::elapsed::doc, request::elapsed::signature.al());
    module_defun(req_ptr, request::cookies::name, request::cookies::func, request::cookies::doc, request::cookies::signature.al());

    return Mrequest;
}
