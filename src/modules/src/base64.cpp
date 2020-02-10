#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"




ALISP_EXPORT alisp::env::ModulePtr init_base64(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mbase64 = alisp::module_init("base64");
    // auto base64_ptr = Mrandom.get();

    // alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);
    
    return Mbase64;
}
