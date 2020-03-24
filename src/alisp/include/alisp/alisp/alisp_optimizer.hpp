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

#include <vector>
#include <string>
#include <utility>
#include <iterator>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_prims.hpp"


namespace alisp
{

namespace optimizer
{

namespace detail
{

template<typename... T> inline bool oreq(ALObjectPtr t_obj, T &&... o)
{

    return (eq(t_obj, o) || ...);
}

static bool is_const(ALObjectPtr t_obj)
{

    return pint(t_obj) || pstring(t_obj) || preal(t_obj) || eq(t_obj, Qt) || eq(t_obj, Qnil);
}

template<typename... T> struct Optimizer : T...
{
    Optimizer() = default;
    explicit Optimizer(T... t) : T(std::move(t))... {}

    auto optimize(ALObjectPtr t_list)
    {
        ((t_list = static_cast<T &>(*this).optimize(std::move(t_list))), ...);
        return t_list;
    }
};

struct PrimesInlining
{
    auto optimize(ALObjectPtr t_list)
    {
        if (!(std::size(*t_list) > 1 && psym(t_list->i(0)))) { return t_list; }

        auto head = t_list->i(0);
        if (env::Environment::g_prime_values.count(head->to_string()) > 0)
        {
            t_list->children()[0] = env::Environment::g_prime_values.at(head->to_string());
            return t_list;
        }

        return t_list;
    }
};

struct DeadCode
{
    template<size_t start = 0, size_t off = 0> void remove_consts(ALObjectPtr t_list)
    {
        if (!plist(t_list)) return;

        for (size_t i = start; i < std::size(*t_list) - off; ++i)
        {
            if (is_const(t_list->i(i)))
            {
                t_list->children().erase(std::remove(t_list->begin(), t_list->end(), t_list->i(i)),
                                         t_list->children().end());
            }
        }
    }

    auto optimize(ALObjectPtr t_list)
    {
        if (!(std::size(*t_list) > 1)) { return t_list; }

        if (oreq(t_list->i(0), Qlet, Plet, Qletx, Pletx, Qwhen, Pwhen, Qunless, Punless))
        { remove_consts<2, 1>(t_list); }

        if (oreq(t_list->i(0), Qdolist, Pdolist, Qwhile, Pwhile)) { remove_consts<2, 0>(t_list); }

        if (oreq(t_list->i(0), Qif, Pif)) { remove_consts<3, 1>(t_list); }

        if (oreq(t_list->i(0), Qprogn1, Pprogn1, Qprogn2, Pprogn2)) { remove_consts<3, 0>(t_list); }

        return t_list;
    }
};

struct If
{
    auto optimize(ALObjectPtr t_list)
    {
        if (!(std::size(*t_list) > 1 && (eq(t_list->i(0), Qif) || eq(t_list->i(0), Pif)))) { return t_list; }

        if (is_const(t_list->i(1)))
        {
            if (is_truthy(t_list->i(1))) { return t_list->i(2); }
            else
            {

                if (std::size(*t_list) > 3)
                {
                    auto new_lis = splice(t_list, 3);
                    new_lis->children().insert(std::begin(*new_lis), Pprogn);
                    return new_lis;
                }

                return Qnil;
            }
        }
        return t_list;
    }
};

struct When
{
    auto optimize(ALObjectPtr t_list)
    {
        if (!(std::size(*t_list) > 1 && oreq(t_list->i(0), Qwhen, Pwhen))) { return t_list; }

        if (!(std::size(*t_list) > 2 && is_const(t_list->i(1)))) { return t_list; }

        if (is_truthy(t_list->i(1)))
        {
            auto new_lis = splice(t_list, 2);
            new_lis->children().insert(std::begin(*new_lis), Pprogn);
            return new_lis;
        }

        return t_list;
    }
};

struct Unless
{
    auto optimize(ALObjectPtr t_list)
    {
        if (!(std::size(*t_list) > 1 && oreq(t_list->i(0), Qunless, Punless))) { return t_list; }

        if (!(std::size(*t_list) > 2 && is_const(t_list->i(1)))) { return t_list; }

        if (is_falsy(t_list->i(1)))
        {
            auto new_lis = splice(t_list, 2);
            new_lis->children().insert(std::begin(*new_lis), Pprogn);
            return new_lis;
        }
        return t_list;
    }
};

struct ConstantFolding
{

    auto optimize(ALObjectPtr t_list) { return t_list; }
};

}  // namespace detail

typedef detail::
  Optimizer<detail::DeadCode, detail::If, detail::When, detail::Unless, detail::ConstantFolding, detail::PrimesInlining>
    PipelineOptimizer;

class MainOptimizer
{
  private:
    PipelineOptimizer m_opt;

    ALObjectPtr do_optimize(ALObjectPtr t_obj);

  public:
    MainOptimizer() : m_opt() {}


    void optimize(std::vector<ALObjectPtr> &t_objs);
};

}  // namespace optimizer

}  // namespace alisp
