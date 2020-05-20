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
#include <thread>

namespace alisp
{

namespace details
{

namespace ch = std::chrono;

typedef std::chrono::duration<ALObject::real_type> al_seconds;

static constexpr int SYSTEM_CLOCK   = 1;
static constexpr int STEADY_CLOCK   = 2;
static constexpr int HIGH_RES_CLOCK = 3;

static constexpr int clocks_per_sec = CLOCKS_PER_SEC;


ALObjectPtr Ftime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));
    std::time_t result = std::time(nullptr);
    return make_int(result);
}

ALObjectPtr Fclock(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(t_obj));
    return make_int(std::clock());
}

ALObjectPtr Fnow(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;

    AL_CHECK(assert_size<1>(t_obj));
    auto clock = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(clock));

    switch (static_cast<int>(clock->to_int()))
    {
        case SYSTEM_CLOCK: {
            const auto time_now = al_seconds(ch::system_clock::now().time_since_epoch()).count();
            return make_real(time_now);
        }

        case STEADY_CLOCK: {
            const auto time_now = al_seconds(ch::steady_clock::now().time_since_epoch()).count();
            return make_real(time_now);
        }

        case HIGH_RES_CLOCK: {
            const auto time_now = al_seconds(ch::high_resolution_clock::now().time_since_epoch()).count();
            return make_real(time_now);
        }
    }

    return nullptr;
}

ALObjectPtr Fnow_ns(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;

    AL_CHECK(assert_size<1>(t_obj));
    auto clock = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(clock));

    switch (static_cast<int>(clock->to_int()))
    {
        case SYSTEM_CLOCK: {
            return make_int(ch::system_clock::now().time_since_epoch().count());
        }

        case STEADY_CLOCK: {
            return make_int(ch::steady_clock::now().time_since_epoch().count());
        }

        case HIGH_RES_CLOCK: {
            return make_int(ch::high_resolution_clock::now().time_since_epoch().count());
        }
    }

    return nullptr;
}

ALObjectPtr Fgmtime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(time));
    std::time_t time_t(time->to_int());
    auto res = std::gmtime(&time_t);
    return make_object(res->tm_sec,
                       res->tm_min,
                       res->tm_hour,
                       res->tm_mday,
                       res->tm_mon,
                       res->tm_year,
                       res->tm_wday,
                       res->tm_yday,
                       res->tm_isdst);
}

ALObjectPtr Flocaltime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(time));
    std::time_t time_t(time->to_int());
    auto res = std::localtime(&time_t);
    return make_object(res->tm_sec,
                       res->tm_min,
                       res->tm_hour,
                       res->tm_mday,
                       res->tm_mon,
                       res->tm_year,
                       res->tm_wday,
                       res->tm_yday,
                       res->tm_isdst);
}

ALObjectPtr Fmktime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time_tup = eval->eval(t_obj->i(0));
    AL_CHECK(assert_list(time_tup));
    AL_CHECK(assert_size<9>(time_tup));
    AL_CHECK(assert_numbers(time_tup));

    std::tm tm;
    tm.tm_sec   = static_cast<int>(time_tup->i(0)->to_int());
    tm.tm_min   = static_cast<int>(time_tup->i(1)->to_int());
    tm.tm_hour  = static_cast<int>(time_tup->i(2)->to_int());
    tm.tm_mday  = static_cast<int>(time_tup->i(3)->to_int());
    tm.tm_mon   = static_cast<int>(time_tup->i(4)->to_int());
    tm.tm_year  = static_cast<int>(time_tup->i(5)->to_int());
    tm.tm_wday  = static_cast<int>(time_tup->i(6)->to_int());
    tm.tm_yday  = static_cast<int>(time_tup->i(7)->to_int());
    tm.tm_isdst = static_cast<int>(time_tup->i(8)->to_int());

    return make_int(std::mktime(&tm));
}

ALObjectPtr Fstrftime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_min_size<2>(t_obj));

    auto time_fmt = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(time_fmt));
    
    auto time_tup = eval->eval(t_obj->i(1));
    AL_CHECK(assert_list(time_tup));
    AL_CHECK(assert_size<9>(time_tup));
    AL_CHECK(assert_numbers(time_tup));

    std::tm tm;
    tm.tm_sec   = static_cast<int>(time_tup->i(0)->to_int());
    tm.tm_min   = static_cast<int>(time_tup->i(1)->to_int());
    tm.tm_hour  = static_cast<int>(time_tup->i(2)->to_int());
    tm.tm_mday  = static_cast<int>(time_tup->i(3)->to_int());
    tm.tm_mon   = static_cast<int>(time_tup->i(4)->to_int());
    tm.tm_year  = static_cast<int>(time_tup->i(5)->to_int());
    tm.tm_wday  = static_cast<int>(time_tup->i(6)->to_int());
    tm.tm_yday  = static_cast<int>(time_tup->i(7)->to_int());
    tm.tm_isdst = static_cast<int>(time_tup->i(8)->to_int());

    char mbstr[512];
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
#endif
    if (auto size = std::strftime(mbstr, sizeof(mbstr), time_fmt->to_string().c_str(), &tm); size != 0)
    {
        return make_string(std::string(mbstr, size));
    }
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif


    return Qnil;
}

ALObjectPtr Fctime(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_max_size<1>(t_obj));
    if (std::size(*t_obj) > 1)
    {
        auto time = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(time));
        std::time_t time_t(time->to_int());
        return make_string(std::ctime(&time_t));
    }
    auto time_t = std::time(nullptr);
    return make_string(std::ctime(&time_t));
}

ALObjectPtr Fns(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(time));
    auto res = ch::duration_cast<al_seconds>(ch::nanoseconds(time->to_int())).count();
    return make_real(res);
}

ALObjectPtr Fms(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(time));
    auto res = ch::duration_cast<al_seconds>(ch::milliseconds(time->to_int())).count();
    return make_real(res);
}

ALObjectPtr Fs(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(time));
    auto res = ch::duration_cast<al_seconds>(ch::seconds(time->to_int())).count();
    return make_real(res);
}

ALObjectPtr Fhr(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(time));
    auto res = ch::duration_cast<al_seconds>(ch::hours(time->to_int())).count();
    return make_real(res);
}

ALObjectPtr Fsleep(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    namespace ch = std::chrono;
    AL_CHECK(assert_size<1>(t_obj));
    auto time = eval->eval(t_obj->i(0));

    std::this_thread::sleep_for(std::chrono::milliseconds(time->to_int()));

    return Qt;
}

}  // namespace details

env::ModulePtr init_time(env::Environment *, eval::Evaluator *)
{
    auto Mtime    = module_init("time");
    auto time_ptr = Mtime.get();

    module_doc(time_ptr,
               R"(The `time` module provides utility functions for working with time
and dates.

The module provides access to several internal clocks of the C++
standard library.
)");

    module_defconst(time_ptr, "system-clock", make_int(details::SYSTEM_CLOCK),
    R"(Integer representing the system-wide real time wall clock.
)");
    
    module_defconst(time_ptr, "steady-clock", make_int(details::STEADY_CLOCK),
    R"(Integer representing a monotonic clock. The time points of this clock
cannot decrease as physical time moves forward and the time between
ticks of this clock is constant.
)");
    
    module_defconst(time_ptr, "high-res-clock", make_int(details::HIGH_RES_CLOCK),
    R"(Integer representing a clock with the smallest tick period provided
by the implementation.
)");
    

    module_defconst(time_ptr, "clocks-pre-second", make_int(details::clocks_per_sec),
    R"(Number of clock ticks per second. Clock ticks are units of time of a
constant but system-specific length.)");
    

    module_defun(time_ptr, "t-time", &details::Ftime,
    R"((t-time)

Return the current calendar time in seconds.
)");
    
    module_defun(time_ptr, "t-ctime", &details::Fctime,
    R"((t-ctime [TIME])

Return a textural representation of the current time. If `TIME` is
given, use this time to construct the string.
)");
    
    module_defun(time_ptr, "t-gmtime", &details::Fgmtime,
    R"((t-gmtime TIME)

Return a list of the form (seconds, minutes, hours month day, month,
year, week day, year day, leap year) representing the time `TIME` as a GM time.
)");
    
    module_defun(time_ptr, "t-localtime", &details::Flocaltime,
    R"((t-localtime TIME)

Return a list of the form (seconds, minutes, hours month day, month,
year, week day, year day, leap year) representing the time `TIME` as a local time.
)");
    
    module_defun(time_ptr, "t-mktime", &details::Fmktime,
    R"((t-mktime TIME-LIST)

Convert a time list of the form (seconds, minutes, hours month day,
month, year, week day, year day, leap year) to time (seconds) since
the beginning of the epoch. The values in the time list are permitted
to be outside their normal ranges.  )");
    
    module_defun(time_ptr, "t-process-time", &details::Fclock,
    R"((t-process-time)

Returns the approximate processor time used by the process since the
beginning of an implementation-defined era related to the program's
execution. To convert result value to seconds divide it by
`clocks-pre-second`.
 )");
    
    module_defun(time_ptr, "t-strftime", &details::Fstrftime,
    R"((t-strftime FORMAT-STRING TIME-LIST)

Return the restulting string by formating `FROMAT-STRING` with the
time list `TIME-LIST`. The ruls for formating are the same as in the
[C++ page for the strftime function ](https://en.cppreference.com/w/cpp/chrono/c/strftime).
The time list is of the form as by the `t-mktime` and `t-gmtime` functions.
)");

    module_defun(time_ptr, "t-clock-time", &details::Fnow,
    R"((t-clock-time CLOCK)

Return the current time in seconds as a real number according to the
given clock. `CLOCK` can be:
* system-clock
* steady-clock
* high-res-clock
 )");
    
    module_defun(time_ptr, "t-clock-time-ns", &details::Fnow_ns,
    R"(Return the current time in nanoseconds as a real number according to
the given clock. `CLOCK` can be:
* system-clock
* steady-clock
* high-res-clock)");
    

    module_defun(time_ptr, "t-ns", &details::Fns,
    R"((t-ns TIME)

Convert `TIME` in nanoseconds to seconds as as real number.
)");
    
    module_defun(time_ptr, "t-ms", &details::Fms,
    R"((t-ms TIME)

Convert `TIME` in miliseconds to seconds as as real number.
)");
    
    module_defun(time_ptr, "t-s", &details::Fs,
    R"((t-s TIME)

Convert `TIME` in seconds to seconds as as real number.
)");
    
    module_defun(time_ptr, "t-hr", &details::Fhr,
    R"((t-hr TIME)

Convert `TIME` in hours to seconds as as real number.
)");
    

    module_defun(time_ptr, "t-sleep", &details::Fsleep,
    R"((t-sleep TIME)

Block the current thread for `TIME` miliseconds.
)");
    

    return Mtime;
}


}  // namespace alisp
