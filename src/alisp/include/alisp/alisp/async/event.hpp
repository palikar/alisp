
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
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/declarations/constants.hpp"

#include <functional>
#include <memory>
#include <utility>

namespace alisp::async
{

using al_callback = std::function<void(ALObjectPtr)>;

class AsyncS;
struct Future;

namespace detail
{


struct AbstractCallback
{
    virtual ALObjectPtr call(AsyncS *async) const = 0;
    virtual ~AbstractCallback()                   = default;
};

template<class T> struct WrappingCallback : AbstractCallback
{
    T cb_;
    explicit WrappingCallback(T &&cb) : cb_(std::move(cb)) {}
    ALObjectPtr call(AsyncS *async) const override { return cb_(async); }
};

struct EventObject
{
    std::unique_ptr<AbstractCallback> ptr_;

    Future *future;

    template<class T> EventObject(T t) { ptr_ = std::make_unique<WrappingCallback<T>>(std::move(t)); }

    ALObjectPtr operator()(AsyncS *async) const { return ptr_->call(async); }
};

struct CallbackObject
{
    ALObjectPtr function;
    ALObjectPtr arguments;

    al_callback internal{};
};

}  // namespace detail

}
