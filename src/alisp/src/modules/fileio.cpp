#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{



ALObjectPtr Fread_file(ALObjectPtr, env::Environment*, eval::Evaluator*)
{
    std::cout << "reading file in the fun" << "\n";
    return Qnil;
}



std::shared_ptr<env::Module> init_fileio(env::Environment*, eval::Evaluator*) {

    auto Mfileio = module_init("fileio");

    
    module_defun(Mfileio.get(), "read-file", &Fread_file);

    module_defvar(Mfileio.get(), "file-separator", make_string("this-is-file-sep"));

    module_defconst(Mfileio.get(), "temp-dir", make_string("/tmp/"));
    

    return Mfileio;
    
}


}









