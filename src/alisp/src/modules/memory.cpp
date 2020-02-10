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


#include "alisp/alisp/alisp_module_helpers.hpp"

#include "alisp/utility/env.hpp"

namespace alisp
{

namespace detail
{



ALObjectPtr Fmmap(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fget_size(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fset_nth_byte(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fget_nth_byte(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fget_range(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Ffill_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fset_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fget_bytes(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}

ALObjectPtr Fallocate_buffer(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto path = eval->eval(t_obj->i(0));
    assert_stream(path);

    return Qnil;
}


}

env::ModulePtr init_memory(env::Environment *, eval::Evaluator *)
{

    auto Mmemory = module_init("memory");
    auto mem_ptr = Mmemory.get();

    
    
    return Mmemory;
}


}  // namespace alisp
