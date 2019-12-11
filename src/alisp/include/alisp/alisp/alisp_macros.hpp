#pragma once

#define PRIMITIVE_CAT(a, ...) a ## __VA_ARGS__
#define CAT(a, ...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define DEFSYM(var, sym_name)                                           \
    inline auto var = &env::Environment::g_global_symbol_table.insert({sym_name, ALObject(sym_name, true)}).first->second

#define DEFVAR(var, sym_name)                                           \
    inline auto var = &env::Environment::g_global_symbol_table.insert({sym_name, ALObject(sym_name, true)}).first->second; \
    inline auto V_ ## var = env::Environment::g_prime_values.insert({sym_name, ALObject({var})})


#define DEFUN(name, sym)                                                \
    extern ALObject* F##name (ALObject*, env::Environment*, eval::Evaluator*); \
    inline auto Q##name = &env::Environment::g_global_symbol_table.insert({sym, ALObject(sym, true)}).first->second; \
    inline auto P##name = env::Environment::g_prime_values.insert({sym, ALOBject(ALOBject::list_type{}).make_prime(&F##name)})
