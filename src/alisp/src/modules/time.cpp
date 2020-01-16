#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{


env::ModulePtr init_time(env::Environment*, eval::Evaluator*) {

    auto Mtime = module_init("time");
    return Mtime;
}


}
