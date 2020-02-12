#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"


alisp::ALObjectPtr Fparse_xml(alisp::ALObjectPtr, alisp::env::Environment *, alisp::eval::Evaluator *)
{
    std::cout << "parsing xml now!"
              << "\n";
    return alisp::Qt;
}


ALISP_EXPORT alisp::env::ModulePtr init_xml(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mxml    = alisp::module_init("xml");
    auto xml_ptr = Mxml.get();

    alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);
    alisp::module_defun(xml_ptr, "parse", &Fparse_xml);

    return Mxml;
}
