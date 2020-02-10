#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"




ALISP_EXPORT alisp::env::ModulePtr init_re(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mre = alisp::module_init("re");
    // auto re_ptr = Mxml.get();

    // alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);

    return Mre;
}
