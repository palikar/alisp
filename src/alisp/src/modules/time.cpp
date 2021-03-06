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

namespace detail
{

namespace ch = std::chrono;

typedef std::chrono::duration<ALObject::real_type> al_seconds;

static constexpr int SYSTEM_CLOCK   = 1;
static constexpr int STEADY_CLOCK   = 2;
static constexpr int HIGH_RES_CLOCK = 3;

static constexpr int clocks_per_sec = CLOCKS_PER_SEC;


struct time
{
    static inline const std::string name{ "time" };

    static inline const std::string doc{ R"(The `time` module provides utility functions for working with time
and dates.

The module provides access to several internal clocks of the C++
standard library.
)" };

    inline static const Signature signature{};

    static ALObjectPtr func(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        std::time_t result = std::time(nullptr);

        if (result == -1)
        {
            return Qnil;
        }

        return make_int(result);
    }
};

struct clock
{
    static inline const std::string name{ "t-process-time" };

    static inline const std::string doc{ R"(((t-process-time)

Returns the approximate processor time used by the process since the
beginning of an implementation-defined era related to the program's
execution. To convert result value to seconds divide it by
`clocks-pre-second`.
 ))" };

    inline static const Signature signature{};

    static ALObjectPtr func(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        return make_int(std::clock());
    }
};

struct now
{
    static inline const std::string name{ "t-clock-time" };

    static inline const std::string doc{ R"((t-clock-time CLOCK)

Return the current time in seconds as a real number according to the
given clock. `CLOCK` can be:
* system-clock
* steady-clock
* high-res-clock
 )" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;

        auto clock = arg_eval(eval, obj, 0);

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
};

struct now_ns
{
    static inline const std::string name{ "t-clock-time-ns" };

    static inline const std::string doc{ R"((t-clock-time-ns CLOCK)

Return the current time in nanoseconds as a real number according to
the given clock. `CLOCK` can be:
* system-clock
* steady-clock
* high-res-clock)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;

        auto clock = arg_eval(eval, obj, 0);

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
};

struct gmtime
{
    static inline const std::string name{ "t-gmtime" };

    static inline const std::string doc{ R"((t-gmtime TIME)

Return a list of the form (seconds, minutes, hours month day, month,
year, week day, year day, leap year) representing the time `TIME` as a GM time.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);
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
};

struct localtime
{
    static inline const std::string name{ "t-localtime" };

    static inline const std::string doc{ R"((t-localtime TIME)

Return a list of the form (seconds, minutes, hours month day, month,
year, week day, year day, leap year) representing the time `TIME` as a local time.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);
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
};

struct mktime
{
    static inline const std::string name{ "t-mktime" };

    static inline const std::string doc{ R"((t-mktime TIME-LIST)

Convert a time list of the form (seconds, minutes, hours month day,
month, year, week day, year day, leap year) to time (seconds) since
the beginning of the epoch. The values in the time list are permitted
to be outside their normal ranges.  )" };

    inline static const Signature signature{ And{ List{}, Size{ 9 }, Numbers{} } };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch  = std::chrono;
        auto time_tup = arg_eval(eval, obj, 0);

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
};

struct strftime
{
    static inline const std::string name{ "t-strftime" };

    static inline const std::string doc{ R"((t-strftime FORMAT-STRING TIME-LIST)

Return the restulting string by formating `FROMAT-STRING` with the
time list `TIME-LIST`. The ruls for formating are the same as in the
[C++ page for the strftime function ](https://en.cppreference.com/w/cpp/chrono/c/strftime).
The time list is of the form as by the `t-mktime` and `t-gmtime` functions.
)" };

    inline static const Signature signature{ String{}, And{ List{}, Size{ 9 }, Numbers{} } };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;

        auto time_fmt = arg_eval(eval, obj, 0);

        auto time_tup = arg_eval(eval, obj, 1);

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
};

struct ctime
{
    static inline const std::string name{ "t-ctime" };

    static inline const std::string doc{ R"((t-ctime [TIME])

Return a textural representation of the current time. If `TIME` is
given, use this time to construct the string.
)" };

    inline static const Signature signature{ Optional{}, Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        if (std::size(*obj) > 1)
        {
            auto time = arg_eval(eval, obj, 0);
            std::time_t time_t(time->to_int());
            return make_string(std::ctime(&time_t));
        }
        auto time_t = std::time(nullptr);
        return make_string(std::ctime(&time_t));
    }
};

struct ns
{
    static inline const std::string name{ "t-ns" };

    static inline const std::string doc{ R"((t-ns TIME)

Convert `TIME` in nanoseconds to seconds as as real number.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);
        auto res     = ch::duration_cast<al_seconds>(ch::nanoseconds(time->to_int())).count();
        return make_real(res);
    }
};

struct ms
{
    static inline const std::string name{ "t-ms" };

    static inline const std::string doc{ R"((t-ms TIME)

Convert `TIME` in miliseconds to seconds as as real number.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);
        auto res     = ch::duration_cast<al_seconds>(ch::milliseconds(time->to_int())).count();
        return make_real(res);
    }
};

struct s
{
    static inline const std::string name{ "t-strftime" };

    static inline const std::string doc{ R"((t-strftime FORMAT-STRING TIME-LIST)

Return the restulting string by formating `FROMAT-STRING` with the
time list `TIME-LIST`. The ruls for formating are the same as in the
[C++ page for the strftime function ](https://en.cppreference.com/w/cpp/chrono/c/strftime).
The time list is of the form as by the `t-mktime` and `t-gmtime` functions.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);
        auto res     = ch::duration_cast<al_seconds>(ch::seconds(time->to_int())).count();
        return make_real(res);
    }
};

struct hr
{
    static inline const std::string name{ "t-hr" };

    static inline const std::string doc{ R"((t-hr TIME)

Convert `TIME` in hours to seconds as as real number.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);
        auto res     = ch::duration_cast<al_seconds>(ch::hours(time->to_int())).count();
        return make_real(res);
    }
};

struct sleep
{
    static inline const std::string name{ "t-sleep" };

    static inline const std::string doc{ R"((t-sleep TIME)

Block the current thread for `TIME` miliseconds.
)" };

    inline static const Signature signature{ Int{} };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace ch = std::chrono;
        auto time    = arg_eval(eval, obj, 0);

        std::this_thread::sleep_for(std::chrono::milliseconds(time->to_int()));

        return Qt;
    }
};


struct system_clock_var
{

    static inline const std::string name{ "system-clock" };

    static inline const std::string doc{ R"(Integer representing the system-wide real time wall clock.
)" };

    static inline const auto var = make_int(SYSTEM_CLOCK);
};

struct steady_clock_var
{

    static inline const std::string name{ "steady-clock" };

    static inline const std::string doc{ R"(Integer representing a monotonic clock. The time points of this clock
cannot decrease as physical time moves forward and the time between
ticks of this clock is constant.
)" };

    static inline const auto var = make_int(STEADY_CLOCK);
};

struct high_res_clock_var
{

    static inline const std::string name{ "high-res-clock" };

    static inline const std::string doc{ R"(Integer representing a clock with the smallest tick period provided
by the implementation.
)" };

    static inline const auto var = make_int(HIGH_RES_CLOCK);
};

struct clocks_per_sec_var
{

    static inline const std::string name{ "clocks-per-second" };

    static inline const std::string doc{ R"(Number of clock ticks per second. Clock ticks are units of time of a
constant but system-specific length.)" };

    static inline const auto var = make_int(clocks_per_sec);
};

struct module_doc
{
    inline static const std::string doc{ R"(The `time` module provides utility functions for working with time
and dates.

The module provides access to several internal clocks of the C++
standard library.
)" };
};

}  // namespace detail

env::ModulePtr init_time(env::Environment *, eval::Evaluator *)
{
    auto Mtime    = module_init("time");
    auto time_ptr = Mtime.get();

    module_doc(time_ptr, detail::module_doc::doc);

    using namespace detail;

    module_defvar(time_ptr, system_clock_var::name, system_clock_var::var);
    module_defvar(time_ptr, steady_clock_var::name, steady_clock_var::var);
    module_defvar(time_ptr, high_res_clock_var::name, high_res_clock_var::var);

    module_defvar(time_ptr, clocks_per_sec_var::name, clocks_per_sec_var::var);

    module_defun(time_ptr, time::name, time::func, time::doc, time::signature.al());
    module_defun(time_ptr, clock::name, clock::func, clock::doc, clock::signature.al());
    module_defun(time_ptr, now::name, now::func, now::doc, now::signature.al());
    module_defun(time_ptr, now_ns::name, now_ns::func, now_ns::doc, now_ns::signature.al());
    module_defun(time_ptr, gmtime::name, gmtime::func, gmtime::doc, gmtime::signature.al());
    module_defun(time_ptr, localtime::name, localtime::func, localtime::doc, localtime::signature.al());
    module_defun(time_ptr, mktime::name, mktime::func, mktime::doc, mktime::signature.al());
    module_defun(time_ptr, strftime::name, strftime::func, strftime::doc, strftime::signature.al());
    module_defun(time_ptr, ctime::name, ctime::func, ctime::doc, ctime::signature.al());
    module_defun(time_ptr, ns::name, ns::func, ns::doc, ns::signature.al());
    module_defun(time_ptr, ms::name, ms::func, ms::doc, ms::signature.al());
    module_defun(time_ptr, s::name, s::func, s::doc, s::signature.al());
    module_defun(time_ptr, hr::name, hr::func, hr::doc, hr::signature.al());
    module_defun(time_ptr, sleep::name, sleep::func, sleep::doc, sleep::signature.al());


    return Mtime;
}


}  // namespace alisp
