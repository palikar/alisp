#include "alisp/alisp/alisp_modules.hpp"


#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_declarations.hpp"



namespace alisp
{


ALObjectPtr Fread_file(ALObjectPtr obj, env::Environment* env, eval::Evaluator* eval)
{

    std::cout << "reading file in the fun" << "\n";

    return Qnil;
}



std::shared_ptr<env::Module> init_fileio(env::Environment*, eval::Evaluator*) {

    
    auto fileio_mod = std::make_shared<env::Module>("fileio");

    fileio_mod->get_root().insert({"read-file", make_prime(&Fread_file, "read-file")});
    fileio_mod->get_root().insert({"file-spearator", make_string("this-is-file-sep")});



    for (auto& [name, sym] : fileio_mod->get_root()) {
        sym->set_prop("--module--", make_string("fileio"));
    }


    return fileio_mod;
}


// inline auto Ffielio = env::Environment::g_builtin_modules.insert({"fileio", env::ModuleImport{&init_fileio}}).first->second;



}









