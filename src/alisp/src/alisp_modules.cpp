
#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_macros.hpp"
#include "alisp/alisp/alisp_factory.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"


namespace alisp {


extern std::shared_ptr<env::Module> init_fileio(env::Environment*, eval::Evaluator*);


void init_modules() {
    env::Environment::g_builtin_modules.insert({"fileio", env::ModuleImport{&init_fileio}});    

}



}
