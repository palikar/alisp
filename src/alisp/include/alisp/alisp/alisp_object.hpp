#pragma once

#include <algorithm>
#include <string>
#include <sstream>
#include <variant>
#include <vector>
#include <iterator>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/utility.hpp"


namespace alisp
{

/*   ____                _   _               _          _                 */
/*  / ___|_ __ ___  __ _| |_(_) ___  _ __   | |__   ___| |_ __  _ __ ___  */
/* | |   | '__/ _ \/ _` | __| |/ _ \| '_ \  | '_ \ / _ \ | '_ \| '__/ __| */
/* | |___| | |  __/ (_| | |_| | (_) | | | | | | | |  __/ | |_) | |  \__ \ */
/*  \____|_|  \___|\__,_|\__|_|\___/|_| |_| |_| |_|\___|_| .__/|_|  |___/ */
/*                                                       |_|   */

namespace detail
{

struct ALObjectHelper
{

    template<typename T>
    static auto get(T a) -> typename  std::enable_if_t<std::is_integral_v<T>, ALObjectPtr> {
        const auto val = static_cast<ALObject::int_type>(a); 
        auto obj = new ALObject(val);

        if (0 <= val && val <= 127) { obj->set_char_flag(); }
        
        return obj;
    }


    template<typename T>
    static auto get(T a) -> typename std::enable_if_t<std::is_floating_point_v<T>, ALObjectPtr> {
        return new ALObject(static_cast<ALObject::real_type>(a));
    }

    
    template<typename T>
    static auto get(T a) -> typename std::enable_if_t<std::is_constructible_v<std::string, T>, ALObjectPtr> {
        return new ALObject(std::string(a));
    }

    template<typename T>
    static auto get(T a, bool) -> typename std::enable_if_t<std::is_constructible_v<std::string, T>, ALObjectPtr> {
        return new ALObject(std::string(a), true);
    }


    static auto get(std::vector<ALObjectPtr> vec_objs) {
        return new ALObject(vec_objs);
    }


    static auto get(ALObjectPtr obj) {
        return obj;
    }

    template<typename ...T>
    static auto get(T... objs) {
        std::vector<ALObjectPtr> vec_objs;
        vec_objs.reserve(sizeof...(objs));
        (vec_objs.push_back(ALObjectHelper::get(objs)), ...);
        return new ALObject(vec_objs);
    }

};

}

template<typename ... T>
inline auto make_object(T && ... args)
{
    return detail::ALObjectHelper::get(std::forward<T>(args) ...);
}

template<typename T>
inline auto make_symbol(T name)
{
    static_assert(std::is_constructible_v<std::string, T>, "Name must be string like type.");
    
    return detail::ALObjectHelper::get(name, true);
}

template<typename T>
inline auto make_int(T value)
{
    static_assert(std::is_integral_v<T>, "Value must be of integer type");

    auto obj = make_object(static_cast<ALObject::int_type>(value));
    return obj;
}

template<typename T>
inline auto make_double(T value)
{
    static_assert(std::is_arithmetic_v<T>, "Value must be of real type");
    return make_object(static_cast<double>(value));
}

inline auto make_string(std::string value)
{
    return make_object(value);
}

inline auto make_list(ALObjectPtr obj)
{
    return detail::ALObjectHelper::get(std::vector{obj});
}

inline auto splice(ALObjectPtr t_obj, std::vector<ALObject>::difference_type start_index,
                   std::vector<ALObject>::difference_type end_index = -1){

    const auto size = static_cast<std::vector<ALObject>::difference_type>(std::size(t_obj->children()));
    const auto end_move = end_index == -1 ? size : end_index;

    auto begin_it = std::next(std::begin(t_obj->children()),  start_index);
    auto end_it = std::next(std::begin(t_obj->children()), end_move);

    if (begin_it > end_it) {
        return  Qnil;
    }

    const auto new_child = std::vector<ALObjectPtr>(begin_it, end_it);
    return make_object(new_child);
}


inline std::string dump(ALObjectPtr obj)
{
    std::ostringstream str;

    switch(obj->type())
    {
      case ALObjectType::INT_VALUE:
          str << obj->to_int() << " ";
          break;

      case ALObjectType::REAL_VALUE:
          str << obj->to_real() << " ";
          break;

      case ALObjectType::STRING_VALUE:
          str << "\"" << obj->to_string() << "\"" << " ";
          break;

      case ALObjectType::SYMBOL:
          str << obj->to_string() << " ";
          break;

      case ALObjectType::LIST:
          str << "(";
          for (auto ob : obj->children())
          {
              str << dump(ob);
          }
          str.seekp(-1, std::ios_base::end);
          str << ") ";
          break;
    }

    return str.str();
}



/*  _     _     _    */
/* | |   (_)___| |_  */
/* | |   | / __| __| */
/* | |___| \__ \ |_  */
/* |_____|_|___/\__| */
/*                        _                   _ _   _              */
/*  _ __ ___   __ _ _ __ (_)_ __  _   _  __ _| | |_(_) ___  _ __   */
/* | '_ ` _ \ / _` | '_ \| | '_ \| | | |/ _` | | __| |/ _ \| '_ \  */
/* | | | | | | (_| | | | | | |_) | |_| | (_| | | |_| | (_) | | | | */
/* |_| |_| |_|\__,_|_| |_|_| .__/ \__,_|\__,_|_|\__|_|\___/|_| |_| */
/*                         |_|  */



inline ALObjectPtr eval_list (eval::Evaluator* evl, ALObjectPtr t_obj, size_t t_offset = 0) {
    auto objects = t_obj->children();
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
    auto start_it = std::next(std::begin(objects), hops);
    auto end_it = std::prev(std::end(objects));

    while (start_it != end_it) {
        evl->eval(*start_it);
        start_it = std::next(start_it);
    }
    return evl->eval(*end_it);
}

template<size_t N>
inline ALObjectPtr eval_list_n (eval::Evaluator* evl, ALObjectPtr t_obj, size_t t_offset = 0) {

    auto objects = t_obj->children();
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
    constexpr auto return_hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(N);

    auto start_it = std::next(std::begin(objects), hops);
    auto return_it = std::next(std::begin(objects), return_hops - 1);
    auto end_it = std::end(objects);

    while (start_it != return_it) {
        evl->eval(*start_it);
        start_it = std::next(start_it);
    }
    auto res = evl->eval(*start_it);
    start_it = std::next(start_it);

    while (start_it != end_it) {
        evl->eval(*start_it);
        start_it = std::next(start_it);
    }

    return res;
}

inline ALObjectPtr eval_list_1 (eval::Evaluator* evl, ALObjectPtr t_obj, size_t t_offset = 0) {
    return eval_list_n<0>(evl, t_obj, t_offset);
}

inline ALObjectPtr eval_list_2 (eval::Evaluator* evl, ALObjectPtr t_obj, size_t t_offset = 0) {
    return eval_list_n<1>(evl, t_obj, t_offset);
}

template<bool eval, typename Callable>
inline auto apply (eval::Evaluator* evl, ALObjectPtr t_obj, Callable t_fun, size_t t_offset = 0){

    auto objects = t_obj->children();
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);

    auto start_it = std::next(std::begin(objects), hops);
    auto end_it = std::prev(std::end(objects));

    while (start_it != end_it) {
        if constexpr (eval){
            t_fun(evl->eval(*start_it));
        } else {
            t_fun(*start_it);
        }
        ++start_it;
    }

    if constexpr (eval){
        return t_fun(evl->eval(*end_it));
    } else {
        return t_fun(*end_it);
    }

}

template<bool eval, typename Callable, typename StartType>
inline StartType reduce ([[maybe_unused]]eval::Evaluator* evl, ALObjectPtr t_obj, Callable && t_fun, StartType t_start, size_t t_offset = 0)
{
    auto objects = t_obj->children();
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);

    auto start_it = std::next(std::begin(objects), hops);
    auto end_it = std::end(objects);

    StartType val =
        [&](){
            if constexpr (eval){
                return t_fun(t_start, evl->eval(*start_it++));
            } else {
                return t_fun(t_start, *start_it++);
            }
        }();


    while (start_it != end_it) {

        if constexpr (eval){
            val = t_fun(val, evl->eval(*start_it));
        } else {
            val = t_fun(val, *start_it);
        }

        start_it = std::next(start_it);
    }

    return val;
}

inline ALObjectPtr eval_transform (eval::Evaluator* evl, ALObjectPtr t_obj, size_t t_offset = 0)
{
    auto objects = t_obj->children();
    const auto hops = static_cast<std::iterator_traits<decltype(std::begin(objects))>::difference_type>(t_offset);
    auto start_it = std::next(std::begin(objects), hops);
    auto end_it = std::end(objects);

    std::vector<ALObjectPtr> new_child;

    while (start_it != end_it) {
        new_child.push_back(evl->eval(*start_it));
        start_it = std::next(start_it);
    }

    return make_object(new_child);
}

/*  ____              _         _   _ _      */
/* | __ )  ___   ___ | |  _   _| |_(_) |___  */
/* |  _ \ / _ \ / _ \| | | | | | __| | / __| */
/* | |_) | (_) | (_) | | | |_| | |_| | \__ \ */
/* |____/ \___/ \___/|_|  \__,_|\__|_|_|___/ */



inline bool is_falsy(ALObjectPtr obj)
{
    if(obj == Qnil) return true;

    if(obj->type() == ALObjectType::LIST) return obj->length() == 0;
    if(obj->type() == ALObjectType::STRING_VALUE) return obj->to_string().empty();
    if(obj->type() == ALObjectType::INT_VALUE) return obj->to_int() == 0;
    if(obj->type() == ALObjectType::REAL_VALUE) return obj->to_real() == 0.0;

    return false;

}

inline bool is_truthy(ALObjectPtr obj)
{
    return !is_falsy(obj);
}

inline bool are_objects_numbers(ALObjectPtr obj)
{
    if (!obj->is_list()) { return obj->is_int() && obj->is_real(); }
    for (auto child : obj->children()) {
        if (!(child->is_int() || child->is_real())) return false;
    }
    return true;
}

inline bool are_objects_int(ALObjectPtr obj)
{
    if (!obj->is_list()) { return obj->is_int(); }
    for (auto child : obj->children()) {
        if (!child->is_int()) return false;
    }
    return true;
}

inline bool are_objects_real(ALObjectPtr obj)
{
    if (!obj->is_list()) { return obj->is_real(); }
    for (auto child : obj->children()) {
        if (!child->is_real()) return false;
    }
    return true;
}

inline bool are_objects_string(ALObjectPtr obj)
{
    if (!obj->is_list()) { return obj->is_string(); }
    for (auto child : obj->children()) {
        if (!child->is_string()) return false;
    }
    return true;
}

inline bool min_list_elements(ALObjectPtr obj, size_t t_element_cnt)
{
    return obj->is_list() && std::size(obj->children()) >= t_element_cnt;
}

inline bool max_list_elements(ALObjectPtr obj, size_t t_element_cnt)
{
    return obj->is_list() && std::size(obj->children()) <= t_element_cnt;
}

inline bool psym(ALObjectPtr obj)
{
    return obj->is_sym();
}

inline bool pint(ALObjectPtr obj)
{
    return obj->is_int();
}

inline bool preal(ALObjectPtr obj)
{
    return obj->is_real();
}

inline bool plist(ALObjectPtr obj)
{
    return obj->is_list();
}

inline bool pstring(ALObjectPtr obj)
{
    return obj->is_string();
}

inline bool pfunction(ALObjectPtr obj)
{
    return obj->check_function_flag();
}



inline const auto AND_OBJ_FUN = [](bool t_acc, ALObjectPtr t_obj) {return t_acc and is_truthy(t_obj);};
inline const auto OR_OBJ_FUN = [](bool t_acc, ALObjectPtr t_obj) {return t_acc or is_truthy(t_obj);};



/*  __  __       _   _             _   _ _      */
/* |  \/  | __ _| |_| |__    _   _| |_(_) |___  */
/* | |\/| |/ _` | __| '_ \  | | | | __| | / __| */
/* | |  | | (_| | |_| | | | | |_| | |_| | \__ \ */
/* |_|  |_|\__,_|\__|_| |_|  \__,_|\__|_|_|___/ */



inline const auto ADD_OBJ_FUN = [](int64_t t_acc, ALObjectPtr t_obj) {return t_acc + t_obj->to_int();};
inline const auto SUB_OBJ_FUN = [](int64_t t_acc, ALObjectPtr t_obj) {return t_acc - t_obj->to_int();};
inline const auto MUL_OBJ_FUN = [](int64_t t_acc, ALObjectPtr t_obj) {return t_acc * t_obj->to_int();};
inline const auto DIV_OBJ_FUN = [](int64_t t_acc, ALObjectPtr t_obj) {return t_acc / t_obj->to_int();};

inline const auto ADD_OBJ_FUN_D = [](double t_acc, ALObjectPtr t_obj) {return t_acc + t_obj->to_real();};
inline const auto SUB_OBJ_FUN_D = [](double t_acc, ALObjectPtr t_obj) {return t_acc - t_obj->to_real();};
inline const auto MUL_OBJ_FUN_D = [](double t_acc, ALObjectPtr t_obj) {return t_acc * t_obj->to_real();};
inline const auto DIV_OBJ_FUN_D = [](double t_acc, ALObjectPtr t_obj) {return t_acc / t_obj->to_real();};


inline const auto SHIFT_LEFT = [](ALObjectPtr t_lhs, ALObjectPtr t_rhs) { return t_lhs->to_int() << t_rhs->to_int();};
inline const auto SHIFT_RIGHT = [](ALObjectPtr t_lhs, ALObjectPtr t_rhs) { return t_lhs->to_int() >> t_rhs->to_int();};
inline const auto BIT_OR = [](ALObjectPtr t_lhs, ALObjectPtr t_rhs) { return t_lhs->to_int() | t_rhs->to_int();};
inline const auto BIT_AND = [](ALObjectPtr t_lhs, ALObjectPtr t_rhs) { return t_lhs->to_int() & t_rhs->to_int();};
inline const auto BIT_XOR = [](ALObjectPtr t_lhs, ALObjectPtr t_rhs) { return t_lhs->to_int() ^ t_rhs->to_int();};
inline const auto BIT_INV = [](ALObjectPtr t_obj) { return ~t_obj->to_int();};


/*  _____                  _ _ _          */
/* | ____|__ _ _   _  __ _| (_) |_ _   _  */
/* |  _| / _` | | | |/ _` | | | __| | | | */
/* | |__| (_| | |_| | (_| | | | |_| |_| | */
/* |_____\__, |\__,_|\__,_|_|_|\__|\__, | */
/*          |_|                    |___/  */

inline bool equal(ALObjectPtr t_lhs, ALObjectPtr t_rhs);


inline bool list_equal(ALObjectPtr t_lhs, ALObjectPtr t_rhs)
{

    auto& children_1 = t_lhs->children();
    auto& children_2 = t_rhs->children();

    if (std::size(children_1) != std::size(children_2) ) { return false; }

    size_t index = 0;
    for (index = 0; index < std::size(children_1); ++index) {
        if (! equal(children_1[index], children_2[index])) return false;
    }

    return true;
}


inline bool real_equal(ALObjectPtr t_lhs, ALObjectPtr t_rhs)
{
    return t_lhs->to_real() == t_rhs->to_real();
}


inline bool equal(ALObjectPtr t_lhs, ALObjectPtr t_rhs)
{
    
    if ( t_lhs->type() != t_rhs->type()) { return false; }
    
    return  make_visit(t_lhs,
                       type ( ALObjectType::SYMBOL ) or
                       type( ALObjectType::STRING_VALUE ) >>=  [t_rhs](ALObjectPtr t_obj)  { return t_obj->to_string().compare(t_rhs->to_string()) == 0; },
                       type( ALObjectType::INT_VALUE )    >>=  [t_rhs](ALObjectPtr t_obj) { return t_obj->to_int() == t_rhs->to_int(); },
                       type( ALObjectType::REAL_VALUE )   >>=  [t_rhs](ALObjectPtr t_obj){ return real_equal(t_obj, t_rhs); },
                       type( ALObjectType::LIST )         >>=  [t_rhs](ALObjectPtr t_obj){ return list_equal(t_obj, t_rhs); },
                       any_pattern()                      >>=  [](ALObjectPtr){ return false; });
}

inline bool eq(ALObjectPtr t_lhs, ALObjectPtr t_rhs)
{
    if ( t_lhs->type() != t_rhs->type()) { return false; }
    
    return make_visit(t_lhs,
                      type( ALObjectType::INT_VALUE )    >>=  [t_rhs](ALObjectPtr t_obj) { return t_obj->to_int() == t_rhs->to_int(); },
                      type( ALObjectType::REAL_VALUE )   >>=  [t_rhs](ALObjectPtr t_obj) { return real_equal(t_obj, t_rhs); },
                      type( ALObjectType::STRING_VALUE ) >>=  [t_rhs](ALObjectPtr t_obj) { return t_obj->to_string().compare(t_rhs->to_string()) == 0; },
                      type ( ALObjectType::SYMBOL ) or
                      type( ALObjectType::LIST )         >>=  [t_rhs](ALObjectPtr t_obj) { return t_rhs == t_obj; });
    
}

}

