#pragma once


#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define DEFSYM(var, sym_name)                                           \
    inline auto var = env::Environment::g_global_symbol_table.insert({sym_name, make_symbol(sym_name)}).first->second

#define DEFVAR(sym, var, sym_name, ...)                                 \
    inline auto sym = env::Environment::g_global_symbol_table.insert({sym_name, make_symbol(sym_name)}).first->second; \
    inline auto var = env::Environment::g_prime_values.insert({sym_name, __VA_ARGS__}).first->second


#define DEFUN(name, sym)                                                \
    extern ALObjectPtr F##name (ALObjectPtr, env::Environment*, eval::Evaluator*); \
    inline auto Q##name = env::Environment::g_global_symbol_table.insert({sym, make_symbol(sym)}).first->second; \
    inline auto P##name = env::Environment::g_prime_values.insert({sym, make_object(ALObject::list_type{})->make_prime(&F##name)}).first->second


    
#define APP_FUNCTION_(NAME, FUN, TYPE)                                  \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl) \
    {                                                                   \
        assert_size<0>(obj);                                            \
        assert_number(obj->i(0));                                       \
        return make_##TYPE(FUN(evl->eval(obj->i(0))->to_##TYPE()));     \
    }


#define REAL_APP_FUNCTION(NAME, FUN) APP_FUNCTION_(NAME, FUN, real)

#define INT_APP_FUNCTION(NAME, FUN) APP_FUNCTION_(NAME, FUN, int)

#define STACK_ALLOC_OBJECT(NAME, PTR, ...)                              \
    ALObject NAME(__VA_ARGS__);                                         \
    auto PTR = ::alisp::detail::ALObjectHelper::init_ptr_temp(&NAME)
