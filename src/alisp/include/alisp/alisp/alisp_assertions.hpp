#pragma once

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"


namespace alisp
{

template<size_t N>
inline void assert_min_size (ALObject* obj)
{
    if(!min_list_elements(obj, N))
        throw std::runtime_error("Invalid argument. Must be list with at least " + std::to_string(N) + " elements"); 
}

template<size_t N>
inline void assert_max_size (ALObject* obj)
{
    if(!max_list_elements(obj, N))
        throw std::runtime_error("Invalid argument. Must be list with maximum of " + std::to_string(N) + " elements"); 
}

template<size_t N>
inline void assert_size (ALObject* obj)
{
    if(obj->length() != N)
        throw std::runtime_error("Invalid argument. Must be list with  " + std::to_string(N) + " elements"); 
}
    
inline void assert_numbers (ALObject* obj)
{
    if(!are_objects_numbers(obj))
        throw std::runtime_error("Invalid argument. The list must contain only numbers(real of int)");
}

inline void assert_symbol (ALObject* obj)
{
    if(!obj->is_sym()) throw std::runtime_error("Invalid argument. Object must be symbol");
}

inline void assert_list (ALObject* obj)
{
    if(!obj->is_list() and obj != Qnil) throw std::runtime_error("Invalid argument. Object must be list");
}

inline void assert_number (ALObject* obj)
{
    if(!obj->is_int() and !obj->is_real()) throw std::runtime_error("Invalid argument. Object must be a number");
}

inline void assert_int (ALObject* obj)
{
    if(!obj->is_int()) throw std::runtime_error("Invalid argument. Object must be an integer");
}



}
