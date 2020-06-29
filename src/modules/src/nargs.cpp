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
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_object.hpp"

namespace nargs
{

using namespace alisp;

struct has
{
    inline static const std::string name{ "nargs-has" };

    inline static const std::string doc{ R"()" };

    inline static const Signature signature{ List{}, Sym{} };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));

        auto l          = eval->eval(t_obj->i(0));
        auto col_string = eval->eval(t_obj->i(1));

        AL_CHECK(assert_list(l));
        AL_CHECK(assert_symbol(col_string));

        return contains(l, col_string->to_string()) ? Qt : Qnil;
    }
};

struct next
{

    inline static const std::string name{ "nargs-next" };

    inline static const std::string doc{ R"()" };

    inline static const Signature signature{ List{}, Sym{} };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));

        auto l          = eval->eval(t_obj->i(0));
        auto col_string = eval->eval(t_obj->i(1));

        AL_CHECK(assert_list(l));
        AL_CHECK(assert_symbol(col_string));

        if (auto [it, succ] = get_next(l, col_string->to_string()); succ)
        {
            return it;
        }

        return Qnil;
    }
};

struct truthy
{

    inline static const std::string name{ "nargs-thruty" };

    inline static const std::string doc{ R"()" };

    inline static const Signature signature{ List{}, Sym{} };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));

        auto l          = eval->eval(t_obj->i(0));
        auto col_string = eval->eval(t_obj->i(1));

        AL_CHECK(assert_list(l));
        AL_CHECK(assert_symbol(col_string));

        if (auto [next, succ] = get_next(l, col_string->to_string()); succ)
        {
            return is_truthy(next) ? Qt : Qnil;
        }
        return Qnil;
    }
};

struct falsey
{

    inline static const std::string name{ "nargs-falsey" };

    inline static const std::string doc{ R"()" };

    inline static const Signature signature{ List{}, Sym{} };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(t_obj));

        auto l          = eval->eval(t_obj->i(0));
        auto col_string = eval->eval(t_obj->i(1));

        AL_CHECK(assert_list(l));
        AL_CHECK(assert_symbol(col_string));

        if (auto [next, succ] = get_next(l, col_string->to_string()); succ)
        {
            return !is_truthy(next) ? Qt : Qnil;
        }
        return Qnil;
    }
};


}  // namespace nargs

ALISP_EXPORT alisp::env::ModulePtr init_nargs(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto nargs_module = module_init("nargs");
    auto ngs_ptr      = nargs_module.get();

    module_defun<nargs::has>(ngs_ptr);
    module_defun<nargs::next>(ngs_ptr);
    module_defun<nargs::truthy>(ngs_ptr);
    module_defun<nargs::falsey>(ngs_ptr);

    return nargs_module;
}
