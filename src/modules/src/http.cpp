#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"


ALISP_EXPORT alisp::env::ModulePtr init_http(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mhttp = alisp::module_init("http");
    // auto base64_ptr = Mrandom.get();

    // alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);

    return Mhttp;
}
