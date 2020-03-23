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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_prims.hpp"


namespace alisp
{

namespace optimizer
{

template<typename... T> struct Optimizer : T...
{
    Optimizer() = default;
    explicit Optimizer(T... t) : T(std::move(t))... {}

    auto optimize(ALObjectPtr t_list)
    {
        ((t_list = static_cast<T &>(*this).optimize(std::move( t_list))), ...);
        return  t_list;
    }


    
};


struct PrimesInlining {
    auto optimize(ALObjectPtr t_list)
    {

        return t_list;
    }
};

struct DeadCode {
    
    auto optimize(ALObjectPtr t_list)
    {

        return t_list;
    }
};

struct IfWhenUnless {
    
    auto optimize(ALObjectPtr t_list)
    {

        return t_list;
    }
};

struct ConstantFolding {
    
    auto optimize(ALObjectPtr t_list)
    {

        return t_list;
    }
};


typedef Optimizer<PrimesInlining, DeadCode, IfWhenUnless, ConstantFolding> PipelineOptimizer; 

class MainOptimizer {
  private:
    

  public:
    MainOptimizer() {
        
    }
    

    std::vector<ALObjectPtr> optimize(std::vector<ALObjectPtr>& t_objs);
    
};

}

}
