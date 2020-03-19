#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"

ALISP_EXPORT alisp::env::ModulePtr init_templ([[maybe_unused]] alisp::env::Environment *env,
                                              [[maybe_unused]] alisp::eval::Evaluator *eval)
{
    using namespace alisp;
    auto M     = alisp::module_init("templ");
    auto m_ptr = M.get();


    return M;
}
