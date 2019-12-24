#pragma once

#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define DEFSYM(var, sym_name)                                           \
    inline auto var = &env::Environment::g_global_symbol_table.insert({sym_name, ALObject(sym_name, true)}).first->second

#define DEFVAR(var, sym_name)                                           \
    inline auto var = &env::Environment::g_global_symbol_table.insert({sym_name, ALObject(sym_name, true)}).first->second; \
    inline auto V_ ## var = env::Environment::g_prime_values.insert({sym_name, ALObject({var})})


#define DEFUN(name, sym)                                                \
    extern ALObjectPtr F##name (ALObjectPtr, env::Environment*, eval::Evaluator*); \
    inline auto Q##name = &env::Environment::g_global_symbol_table.insert({sym, ALObject(sym, true)}).first->second; \
    inline auto P##name = env::Environment::g_prime_values.insert({sym, *ALObject(ALObject::list_type{}).make_prime(&F##name)})


    
#define APP_FUNCTION_(NAME, FUN, TYPE)                                  \
    ALObjectPtr NAME(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl) \
    {                                                                   \
        assert_size<0>(obj);                                            \
        assert_number(obj->i(0));                                       \
        return make_##TYPE(FUN(evl->eval(obj->i(0))->to_##TYPE()));     \
    }


#define REAL_APP_FUNCTION(NAME, FUN) APP_FUNCTION_(NAME, FUN, real)
#define INT_APP_FUNCTION(NAME, FUN) APP_FUNCTION_(NAME, FUN, int)
