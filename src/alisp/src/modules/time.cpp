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
    auto time_tup = eval->eval(t_obj->i(1));
    AL_CHECK(assert_list(time_tup));
    AL_CHECK(assert_size<9>(time_tup));
    AL_CHECK(assert_numbers(time_tup));

    auto time_fmt = eval->eval(t_obj->i(0));
    AL_CHECK(assert_string(time_fmt));

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
and dates.)");

    module_defconst(time_ptr, "system-clock", make_int(details::SYSTEM_CLOCK));
    module_defconst(time_ptr, "steady-clock", make_int(details::STEADY_CLOCK));
    module_defconst(time_ptr, "high-res-clock", make_int(details::HIGH_RES_CLOCK));

    module_defconst(time_ptr, "clocks-pre-second", make_int(details::clocks_per_sec));

    module_defun(time_ptr, "t-time", &details::Ftime);
    module_defun(time_ptr, "t-ctime", &details::Fctime);
    module_defun(time_ptr, "t-gmtime", &details::Fgmtime);
    module_defun(time_ptr, "t-localtime", &details::Flocaltime);
    module_defun(time_ptr, "t-mktime", &details::Fmktime);
    module_defun(time_ptr, "t-process-time", &details::Fclock);
    module_defun(time_ptr, "t-strftime", &details::Fstrftime);

    module_defun(time_ptr, "t-clock-time", &details::Fnow);
    module_defun(time_ptr, "t-clock-time-ns", &details::Fnow_ns);

    module_defun(time_ptr, "t-ns", &details::Fns);
    module_defun(time_ptr, "t-ms", &details::Fms);
    module_defun(time_ptr, "t-s", &details::Fs);
    module_defun(time_ptr, "t-hr", &details::Fhr);

    module_defun(time_ptr, "t-sleep", &details::Fsleep);

    return Mtime;
}


}  // namespace alisp
