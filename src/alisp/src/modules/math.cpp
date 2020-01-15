#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{


std::shared_ptr<env::Module> init_math(env::Environment*, eval::Evaluator*) {

    auto Mmath = module_init("math");
    return Mmath;
}


}
