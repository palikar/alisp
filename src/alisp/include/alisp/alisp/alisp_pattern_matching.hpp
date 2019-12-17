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
    bool check(ALObject* obj){ return m_check(obj);}
    void call(ALObject* obj){ m_call(obj);}
};

template<typename Check, typename Call>
pattern_entry<Check, Call> operator>>=(match<Check> t_match, Call && callable)
{
    return pattern_entry(t_match.m_fun, callable);
}

template<typename Callalble_In_1, typename Callalble_In_2>
auto operator||(match<Callalble_In_1> t_match_lhs, match<Callalble_In_2> t_match_rhs)
{
    return detail::match(
        [t_rhs=std::move(t_match_rhs),
         t_lhs=std::move(t_match_lhs)](ALObject* obj)->bool {
            return t_rhs.m_fun(obj) || t_lhs.m_fun(obj); });
}


template<typename Callalble_In_1, typename Callalble_In_2>
auto operator&&(detail::match<Callalble_In_1> t_match_lhs, detail::match<Callalble_In_2> t_match_rhs)
{
    return detail::match(
        [t_rhs=std::move(t_match_rhs),
         t_lhs=std::move(t_match_lhs)](ALObject* obj)->bool {
            return t_rhs.m_fun(obj) && t_lhs.m_fun(obj); });
}

template<typename Callalble_In_1>
auto operator!(match<Callalble_In_1> t_match)
{
    return detail::match(
        [t_match=std::move(t_match)](ALObject* obj)->bool {
            return !t_match.m_fun(obj); });
}


template<size_t N, class ... Matches, class ... Checks>
void visit_match_impl([[maybe_unused]] ALObject* obj, [[maybe_unused]] std::tuple<pattern_entry<Checks, Matches>...> patterns){

    if constexpr (N >= sizeof...(Checks)) {
        return;
    } else {
        if (std::get<N>(patterns).check(obj)) {
            std::get<N>(patterns).call(obj);
        } else{
            visit_match_impl<N+1>(obj, patterns);
        }
    }
}

template <class ... Matches, class ... Checks >
void visit_match(ALObject* obj, std::tuple<pattern_entry<Checks, Matches>...> patterns ){
    visit_match_impl<0>(obj, patterns);
}

}


inline auto type(ALObjectType t_type) {
    return detail::match(
        [t_t=std::move(t_type)](ALObject* obj)->bool {
            return  obj->type() == t_t;});
}

inline auto is_function(){
    return detail::match(
        [](ALObject* obj)->bool {
            return  obj->check_function_flag(); });
}

inline auto is_macro(){
    return detail::match(
        [](ALObject* obj)->bool {
            return  obj->check_macro_flag(); });
}

inline auto is_char(){
    return detail::match(
        [](ALObject* obj)->bool {
            return  obj->check_char_flag(); });
}

inline auto is_const(){
    return detail::match(
        [](ALObject* obj)->bool {
            return  obj->check_const_flag(); });
}

template <class ... Matches, class ... Checks >
inline void make_visit (ALObject* obj, detail::pattern_entry<Matches, Checks> ... entries){
    visit_match(obj, std::tuple(entries ...));
}


}
