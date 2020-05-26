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

#ifdef ALISP_HAS_DECLSPEC
#define ALISP_EXPORT extern "C" __declspec(dllexport)
#else
#define ALISP_EXPORT extern "C"
#endif


#define PRIMITIVE_CAT(a, ...) a##__VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define DEFSYM(var, sym_name, DOC) \
    inline auto var =              \
      env::Environment::g_global_symbol_table.insert({ sym_name, make_symbol(sym_name, DOC) }).first->second

#define DEFVAR(sym, var, sym_name, value, doc)                                                           \
    inline auto sym =                                                                                    \
      env::Environment::g_global_symbol_table.insert({ sym_name, make_symbol(sym_name) }).first->second; \
    inline auto var = env::Environment::g_prime_values.insert({ sym_name, make_doc(value, doc) }).first->second


#define DEFUN(name, sym, doc)                                                                                      \
    extern ALObjectPtr F##name(ALObjectPtr, env::Environment *, eval::Evaluator *);                                \
    inline auto Q##name = env::Environment::g_global_symbol_table.insert({ sym, make_symbol(sym) }).first->second; \
    inline auto P##name = env::Environment::g_prime_values.insert({ sym, make_prime(&F##name, sym, doc) }).first->second


#define APP_FUNCTION_(NAME, FUN, TYPE)                                          \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl) \
    {                                                                           \
        assert_size<1>(obj);                                                    \
        auto num = evl->eval(obj->i(0));                                        \
        assert_number(num);                                                     \
        return make_##TYPE(FUN(num->to_##TYPE()));                              \
    }                                                                           \
    static_assert(true, "")


#define REAL_APP_FUNCTION(NAME, FUN) APP_FUNCTION_(NAME, FUN, real)

#define INT_APP_FUNCTION(NAME, FUN) APP_FUNCTION_(NAME, FUN, int)


#define APP_BIFUNCTION(NAME, FUN)                                                                      \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)                        \
    {                                                                                                  \
        assert_size<2>(obj);                                                                           \
        auto eval_obj = eval_transform(evl, obj);                                                      \
        assert_numbers(eval_obj);                                                                      \
        if (are_objects_int(eval_obj))                                                                 \
        {                                                                                              \
            const ALObject::int_type res = FUN(eval_obj->i(0)->to_int(), eval_obj->i(1)->to_int());    \
            return make_int(res);                                                                      \
        }                                                                                              \
        else if (are_objects_numbers(eval_obj))                                                        \
        {                                                                                              \
            const ALObject::real_type res = FUN(eval_obj->i(0)->to_real(), eval_obj->i(1)->to_real()); \
            return make_double(res);                                                                   \
        }                                                                                              \
        return Qnil;                                                                                   \
    }                                                                                                  \
    static_assert(true, "")


#define INT_APP_BIFUNCTION(NAME, FUN)                                                               \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)                     \
    {                                                                                               \
        assert_size<2>(obj);                                                                        \
        auto eval_obj = eval_transform(evl, obj);                                                   \
        assert_numbers(eval_obj);                                                                   \
        if (are_objects_int(eval_obj))                                                              \
        {                                                                                           \
            const ALObject::int_type res = FUN(eval_obj->i(0)->to_int(), eval_obj->i(1)->to_int()); \
            return make_int(res);                                                                   \
        }                                                                                           \
        return Qnil;                                                                                \
    }                                                                                               \
    static_assert(true, "")


#define REAL_APP_BIFUNCTION(NAME, FUN)                                                                 \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)                        \
    {                                                                                                  \
        assert_size<2>(obj);                                                                           \
        auto eval_obj = eval_transform(evl, obj);                                                      \
        assert_numbers(eval_obj);                                                                      \
        if (are_objects_numbers(eval_obj))                                                             \
        {                                                                                              \
            const ALObject::real_type res = FUN(eval_obj->i(0)->to_real(), eval_obj->i(1)->to_real()); \
            return make_double(res);                                                                   \
        }                                                                                              \
        return Qnil;                                                                                   \
    }                                                                                                  \
    static_assert(true, "")


#define REAL_APP_PREDICATE(NAME, FUN)                                           \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl) \
    {                                                                           \
        assert_size<1>(obj);                                                    \
        const auto one = evl->eval(obj->i(0));                                  \
        assert_number(one);                                                     \
        return FUN(one->to_real()) ? Qt : Qnil;                                 \
    }                                                                           \
    static_assert(true, "")


#define STACK_ALLOC_OBJECT(NAME, PTR, ...) \
    ALObject NAME(__VA_ARGS__);            \
    auto PTR = ::alisp::detail::ALObjectHelper::init_ptr_temp(&NAME)


#ifdef DISABLE_CHECKS
#define AL_CHECK(EXR) \
    do                \
    {                 \
        (void)0;      \
    } while (false)
#else
#define AL_CHECK(EXR) EXR
#endif


#define AL_EVAL(obj, eval_obj, index) eval_obj->eval(obj->i(index))

#define AL_BOOL(value) value ? Qt : Qnil 
