#include "alisp/alisp/alisp_modules.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"




namespace alisp
{

extern env::ModulePtr init_fileio(env::Environment*, eval::Evaluator*);
extern env::ModulePtr init_system(env::Environment*, eval::Evaluator*);
extern env::ModulePtr init_math(env::Environment*, eval::Evaluator*);
extern env::ModulePtr init_time(env::Environment*, eval::Evaluator*);
extern env::ModulePtr init_platform(env::Environment*, eval::Evaluator*);

namespace env
{

void init_modules() {

    Environment::g_builtin_modules.insert({"fileio", ModuleImport{&init_fileio}});
    Environment::g_builtin_modules.insert({"system", ModuleImport{&init_system}});
    Environment::g_builtin_modules.insert({"math", ModuleImport{&init_math}});
    Environment::g_builtin_modules.insert({"time", ModuleImport{&init_time}});
    Environment::g_builtin_modules.insert({"platform", ModuleImport{&init_time}});

}

}

}
