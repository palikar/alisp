#include <string>
#include <unordered_map>
#include <utility>

#include "tinyxml2.h"

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/utility/files.hpp"
#include "alisp/utility/string_utils.hpp"


namespace xml
{
using namespace tinyxml2;
using namespace alisp;

auto xml_signal = alisp::make_symbol("xml-signal");

namespace detail
{

ALObjectPtr node_to_sexp(const XMLNode* t_node) {
    
    std::unordered_map<std::string, ALObject::list_type> val_map;
    
    for (const auto* child=t_node->FirstChildElement(); child; child=child->NextSiblingElement() ) {

        auto name = ":"s += std::string{child->Name()};

        if (val_map.count(name) == 0) { val_map.insert({name, ALObject::list_type{}}); }
        
        ALObject::list_type sub_list;
        for (const auto* at = child->FirstAttribute(); at; at=at->Next()) {
            sub_list.push_back(make_symbol(":@"s += std::string(at->Name())));
            sub_list.push_back(make_string(std::string(at->Value())));

        }
        
        auto text = child->GetText();
        if (text) {
            sub_list.push_back(make_symbol(":#text"));
            sub_list.push_back(make_string(text));
        }
        auto l = make_list(sub_list);
        l->set_prop("--array--", Qt);
        val_map[name].push_back(std::move(l));
    }

    ALObject::list_type map_list;
    for (auto& [name, list] : val_map) {
        map_list.push_back(make_symbol(name));
        
        if (list.size() == 1 ) {
            auto l = make_object(list[0]);
            l->set_prop("--dict--", Qt);
            map_list.push_back(std::move(l));
        }else {
            auto l = make_object(list);
            l->set_prop("--array--", Qt);
            map_list.push_back(std::move(l));
        }
        
    }
    
    auto res = make_list(map_list);
    res->set_prop("--dict--", Qt);
    return res;
}

ALObjectPtr xml_to_sexp(XMLDocument& t_doc) {

    ALObject::list_type lis; 
    
    for (const auto* child=t_doc.FirstChildElement(); child; child=child->NextSiblingElement() ) {
        auto name = ":"s += std::string{child->Name()};

        for (auto at = child->FirstAttribute(); at; at=at->Next()) {
            auto at_name = ":@"s += std::string(at->Name());
            auto at_value = std::string(at->Value());
            std::cout << at_name << " - " << at_value << "\n";
        }

        auto text = child->GetText();
        if (text) {
            lis.push_back(make_symbol(":#text"));
            lis.push_back(make_string(text));
        }

        auto child_obj = node_to_sexp(child);
        lis.push_back(make_symbol(name));
        lis.push_back(child_obj);
    }

    auto res = make_list(lis);
    res->set_prop("--dict--", Qt);
    return res;
}

ALObjectPtr from_string(const std::string t_input) {
    XMLDocument doc;
    doc.Parse(t_input.c_str());
    return detail::xml_to_sexp(doc);
}

}

ALObjectPtr Fparse_xml(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_string(str);
    return detail::from_string(str->to_string());
}

ALObjectPtr Fload_file(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;

    assert_size<1>(obj);
    auto file = eval->eval(obj->i(0));
    assert_string(file);

    if (!fs::exists(file->to_string())) { return Qnil; }
    if (!fs::is_regular_file(file->to_string())) { return Qnil; }

    return detail::from_string(utility::load_file(file->to_string()));
}

}

ALISP_EXPORT alisp::env::ModulePtr init_xml(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mxml    = alisp::module_init("xml");
    auto xml_ptr = Mxml.get();

    alisp::module_doc(
        xml_ptr,
        R"(The `xml` module enables the handling of XML-formated data. I provides functionality for parsing and dumping s-expressions as XML. )");

    alisp::module_defun(xml_ptr, "xml-parse", &xml::Fparse_xml);
    // alisp::module_defun(xml_ptr, "xml-dump", &xml::Fparse_xml);

    alisp::module_defun(xml_ptr, "load-file", &xml::Fload_file);
    // alisp::module_defun(xml_ptr, "dump-file", &xml::Fparse_xml);

    return Mxml;
}
