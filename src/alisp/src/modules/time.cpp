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
#include <iomanip>
#include <chrono>
#include <ctime>

namespace alisp
{

namespace details
{
namespace ch = std::chrono;

typedef std::chrono::duration<ALObject::real_type> al_seconds;

static constexpr int SYSTEM_CLOCK = 1;
static constexpr int STEADY_CLOCK = 2;
static constexpr int HIGH_RES_CLOCK = 3;
// static constexpr int GPS_CLOCK = 4;
// static constexpr int LOCAL_CLOCK = 4;


ALObjectPtr Fnow(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    
    assert_size<1>(t_obj);
    auto clock = eval->eval(t_obj->i(0));
    assert_int(clock);
    
    switch (static_cast<int>(clock->to_int())) {
      case SYSTEM_CLOCK : {
          const auto time_now = al_seconds(ch::system_clock::now().time_since_epoch()).count();
          return make_real(time_now);
      }

      case STEADY_CLOCK : {
          const auto time_now = al_seconds(ch::steady_clock::now().time_since_epoch()).count();
          return make_real(time_now);
      }

      case HIGH_RES_CLOCK : {
          const auto time_now = al_seconds(ch::high_resolution_clock::now().time_since_epoch()).count();
          return make_real(time_now);
      }
    }

    return nullptr;
    
}

ALObjectPtr Fnow_ns(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    
    assert_size<1>(t_obj);
    auto clock = eval->eval(t_obj->i(0));
    assert_int(clock);
    
    switch (static_cast<int>(clock->to_int())) {
      case SYSTEM_CLOCK : {
          auto res =  make_int(ch::system_clock::now().time_since_epoch().count());
          res->set_prop("--clock--", make_int(SYSTEM_CLOCK));
      }

      case STEADY_CLOCK : {
          break;
      }

      case HIGH_RES_CLOCK : {
          break;
      }
    }

    return nullptr;
    
}

ALObjectPtr Fgmtime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    
    assert_size<1>(t_obj);
    auto time = eval->eval(t_obj->i(0));
    // assert_int(time);


    switch (static_cast<int>(time->get_prop("--clock--")->to_int())) {
      case SYSTEM_CLOCK : {
          auto time_t = ch::system_clock::to_time_t(ch::time_point<ch::system_clock>(ch::duration<ALObject::int_type>(static_cast<ALObject::int_type>(time->to_real()))));
          auto gm = std::gmtime(&time_t);
          std::cout << gm->tm_hour << "\n";
          
      }

      case STEADY_CLOCK : {
          break;
      }

      case HIGH_RES_CLOCK : {
          break;
      }
    }
    
    
    return nullptr;
    
}



}


env::ModulePtr init_time(env::Environment *, eval::Evaluator *)
{

    auto Mtime    = module_init("time");
    auto time_ptr = Mtime.get();

    module_doc(time_ptr, R"(The `time` module provides utility functions for working with time
and dates.)");



    module_defconst(time_ptr, "system-clock", make_int(details::SYSTEM_CLOCK));
    module_defconst(time_ptr, "steady-clock", make_int(details::STEADY_CLOCK));
    module_defconst(time_ptr, "high-res-clock", make_int(details::HIGH_RES_CLOCK));

    module_defun(time_ptr, "t-now", &details::Fnow);
    module_defun(time_ptr, "t-now-ns", &details::Fnow_ns);
    module_defun(time_ptr, "t-gmtime", &details::Fgmtime);

    return Mtime;
}


}  // namespace alisp
