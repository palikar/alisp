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

#include <utility>
#include <tuple>
#include <vector>
#include <iostream>
#include <iterator>

#include "alisp/utility/helpers.hpp"

namespace alisp::utility
{
namespace detail
{

template<typename T, typename... Tracker> struct proxy
{

  public:
    proxy(T &obj, Tracker &&... track) : m_obj(&obj), m_track(std::forward<decltype(track)>(track)...) { (track.before(obj), ...); }

    ~proxy()
    {
        ::alisp::utility::for_each(m_track, [&](auto &tracker) { tracker.after(*m_obj); });
    }

    T *operator->() { return m_obj; }

  private:
    T *m_obj;
    std::tuple<Tracker...> m_track;
};

}  // namespace detail

template<typename T, typename... Tracker> class Tracked : public Tracker...
{
  public:
    explicit Tracked(T &obj) : m_obj(obj) {}

    detail::proxy<T, Tracker...> operator->() { return detail::proxy<T, Tracker...>(m_obj, static_cast<Tracker>(*this)...); }

  private:
    T &m_obj;
};

}  // namespace alisp::utility
