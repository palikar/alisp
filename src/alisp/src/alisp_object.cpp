
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility.hpp"


namespace alisp
{

ALObject* env::intern(const std::string& name)
{
    
    if(env::global_sym.count(name))
    {
        return &env::global_sym.at(name);
        
    }

    if(env::sym.count(name))
    {
        return env::sym.at(name);
    }

    auto[new_sym, insertion] = env::sym.insert({name, make_symbol(name)});
    return new_sym->second;
}


}
