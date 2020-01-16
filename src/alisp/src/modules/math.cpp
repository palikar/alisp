#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{


env::ModulePtr init_math(env::Environment*, eval::Evaluator*) {

    auto Mmath = module_init("math");
    return Mmath;
}


}
