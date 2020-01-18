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

#include <algorithm>
#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <utility>

#include "alisp/alisp/alisp_common.hpp"

namespace alisp
{



/*  ____       _   _                    __  __       _       _     _              */
/* |  _ \ __ _| |_| |_ ___ _ __ _ __   |  \/  | __ _| |_ ___| |__ (_)_ __   __ _  */
/* | |_) / _` | __| __/ _ \ '__| '_ \  | |\/| |/ _` | __/ __| '_ \| | '_ \ / _` | */
/* |  __/ (_| | |_| ||  __/ |  | | | | | |  | | (_| | || (__| | | | | | | | (_| | */
/* |_|   \__,_|\__|\__\___|_|  |_| |_| |_|  |_|\__,_|\__\___|_| |_|_|_| |_|\__, | */
/*                                                                         |___/  */

namespace detail
{

template<typename Callable>
struct match {
  public:
    Callable m_fun;
  public:
    match(Callable && t_fun) : m_fun(std::forward<Callable>(t_fun)){}
};


template<typename Check, typename Call>
struct pattern_entry {
  private:
    Check m_check;
    Call m_call;
  public:
    pattern_entry(Check check, Call call) :
        m_check(check),  m_call(call){}
    bool check(ALObjectPtr obj){ return m_check(obj);}
    auto call(ALObjectPtr obj){ return m_call(obj);}
};

template<typename Check, typename Call>
pattern_entry<Check, Call> operator>>=(match<Check> t_match, Call && callable)
{
    return pattern_entry(t_match.m_fun, callable);
}

template<typename Check, typename Call>
pattern_entry<Check, Call> operator>(match<Check> t_match, Call && callable)
{
    return pattern_entry(t_match.m_fun, callable);
}

template<typename Callalble_In_1, typename Callalble_In_2>
auto operator||(match<Callalble_In_1> t_match_lhs, match<Callalble_In_2> t_match_rhs)
{
    return detail::match(
        [t_rhs=std::move(t_match_rhs),
         t_lhs=std::move(t_match_lhs)](ALObjectPtr obj)->bool {
            return t_rhs.m_fun(obj) || t_lhs.m_fun(obj); });
}


template<typename Callalble_In_1, typename Callalble_In_2>
auto operator&&(detail::match<Callalble_In_1> t_match_lhs, detail::match<Callalble_In_2> t_match_rhs)
{
    return detail::match(
        [t_rhs=std::move(t_match_rhs),
         t_lhs=std::move(t_match_lhs)](ALObjectPtr obj)->bool {
            return t_rhs.m_fun(obj) && t_lhs.m_fun(obj); });
}

template<typename Callalble_In_1>
auto operator!(match<Callalble_In_1> t_match)
{
    return detail::match(
        [t_match=std::move(t_match)](ALObjectPtr obj)->bool {
            return !t_match.m_fun(obj); });
}


template<size_t N, class ... Matches, class ... Checks>
auto visit_match_impl([[maybe_unused]] ALObjectPtr obj, [[maybe_unused]] std::tuple<pattern_entry<Checks, Matches>...> patterns) -> std::common_type_t<std::invoke_result_t<Matches, ALObjectPtr> ...>{


    if constexpr (N >= sizeof...(Checks)) {

        if constexpr (std::is_void_v<std::common_type_t<std::invoke_result_t<Matches, ALObjectPtr> ...>>) {
            return;
        } else {
            return std::common_type_t<std::invoke_result_t<Matches, ALObjectPtr> ...>{};
        }
        
    } else {
        if (std::get<N>(patterns).check(obj)) {
            return std::get<N>(patterns).call(obj);
        } else{
            return visit_match_impl<N+1>(obj, patterns);
        }
    }
}

template <class ... Matches, class ... Checks >
auto visit_match(ALObjectPtr obj, std::tuple<pattern_entry<Checks, Matches>...> patterns ){
    return visit_match_impl<0>(obj, patterns);
}

}


inline auto type(ALObjectType t_type){
    return detail::match(
        [t_t=std::move(t_type)](ALObjectPtr obj)->bool {
            return  obj->type() == t_t;});
}

inline auto is_function(){
    return detail::match(
        [](ALObjectPtr obj)->bool {
            return  obj->check_function_flag(); });
}

inline auto is_macro(){
    return detail::match(
        [](ALObjectPtr obj)->bool {
            return  obj->check_macro_flag(); });
}

inline auto is_char(){
    return detail::match(
        [](ALObjectPtr obj)->bool {
            return  obj->check_char_flag(); });
}

inline auto is_const(){
    return detail::match(
        [](ALObjectPtr obj)->bool {
            return  obj->check_const_flag(); });
}

inline auto any_pattern(){
    return detail::match( [](ALObjectPtr)->bool { return  true; });
}

template <class ... Matches, class ... Checks >
inline auto make_visit (ALObjectPtr obj, detail::pattern_entry<Matches, Checks> ... entries){
    return visit_match(obj, std::tuple(entries ...));
}


}


template<typename T>
inline auto default_return()
{
    static_assert(std::is_default_constructible_v<T>, "The type must be default constructable");
    if constexpr (std::is_void_v<T>) {
        return;
    } else {
        return T{};
    }
}
