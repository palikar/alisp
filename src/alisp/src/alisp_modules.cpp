#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"


namespace alisp {


extern std::shared_ptr<env::Module> init_fileio(env::Environment*, eval::Evaluator*);

// extern std::shared_ptr<env::Module> init_system(env::Environment*, eval::Evaluator*);
// extern std::shared_ptr<env::Module> init_math(env::Environment*, eval::Evaluator*);
// extern std::shared_ptr<env::Module> init_time(env::Environment*, eval::Evaluator*);


void init_modules() {

    env::Environment::g_builtin_modules.insert({"fileio", env::ModuleImport{&init_fileio}});

    // env::Environment::g_builtin_modules.insert({"system", env::ModuleImport{&init_system}});
    // env::Environment::g_builtin_modules.insert({"math", env::ModuleImport{&init_math}});
    // env::Environment::g_builtin_modules.insert({"time", env::ModuleImport{&init_time}});

}



}
