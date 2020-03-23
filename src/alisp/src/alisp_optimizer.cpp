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


#include "alisp/alisp/alisp_optimizer.hpp"


namespace alisp
{
namespace optimizer
{

ALObjectPtr MainOptimizer::do_optimize(ALObjectPtr t_obj)
{
    if (std::size(*t_obj) > 1 && (eq(t_obj->i(0), Qquote) || eq(t_obj->i(0), Qbackquote))) { return t_obj; }

    for (auto el : *t_obj)
    {
        if (!plist(el)) { continue; }
        el = do_optimize(el);
    }

    auto res = m_opt.optimize(t_obj);
    return res;
}

void MainOptimizer::optimize(std::vector<ALObjectPtr> &t_objs)
{

    for (auto &obj : t_objs)
    {
        if (!plist(obj)) continue;
        obj = do_optimize(obj);
    }
}

}  // namespace optimizer

}  // namespace alisp
