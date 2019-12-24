#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/utility.hpp"


namespace alisp
{

ALObjectPtr env::intern(const std::string& name)
{
    
    if(env::Environment::g_global_symbol_table.count(name))
    {
        return &env::Environment::g_global_symbol_table.at(name);
        
    }

    if(env::Environment::g_symbol_table.count(name))
    {
        return env::Environment::g_symbol_table.at(name);
    }

    auto[new_sym, insertion] =
        env::Environment::g_symbol_table.insert({name, make_symbol(name)});
    return new_sym->second;
}


}
