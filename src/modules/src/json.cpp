#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"


ALISP_EXPORT alisp::env::ModulePtr init_json(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mjson = alisp::module_init("json");
    // auto re_ptr = Mxml.get();

    // alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);

    return Mjson;
}
