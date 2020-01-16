#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{


env::ModulePtr init_system(env::Environment*, eval::Evaluator*) {

    auto Msystem = module_init("system");
    return Msystem;
}


}
