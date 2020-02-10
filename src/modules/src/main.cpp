#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"


extern "C" alisp::env::ModulePtr init_test(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    std::cout << "this is initing!"
              << "\n";


    return nullptr;
}
