#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{


env::ModulePtr init_platform(env::Environment*, eval::Evaluator*) {

    auto Mplatform = module_init("system");
    return Mplatform;
}


}
