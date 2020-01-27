#include "alisp/config.hpp"

#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_loadable_modules.hpp"


extern "C" alisp::env::ModulePtr init_test(alisp::env::Environment *, alisp::eval::Evaluator*)
{
    std::cout << "this is initing!" << "\n";
    

    return nullptr;
}

    
