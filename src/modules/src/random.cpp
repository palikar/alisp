#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"




ALISP_EXPORT alisp::env::ModulePtr init_random(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mrandom = alisp::module_init("random");
    // auto xml_ptr = Mrandom.get();

    // alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);
    
    return Mrandom;
}
