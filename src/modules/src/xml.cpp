/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#include <string>
#include <unordered_map>
#include <utility>

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#endif

#include "tinyxml2.h"

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

#include "alisp/config.hpp"
#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/utility/files.hpp"
#include "alisp/utility/string_utils.hpp"

// -Wsign-conversion
namespace xml
{
using namespace tinyxml2;
using namespace alisp;

auto xml_signal = alisp::make_symbol("xml-signal");

namespace detail
{

ALObjectPtr node_to_sexp(const XMLNode *t_node)
{

    std::unordered_map<std::string, ALObject::list_type> val_map;

    for (const auto *child = t_node->FirstChildElement(); child; child = child->NextSiblingElement())
    {

        auto name = ":"s += std::string{ child->Name() };

        if (val_map.count(name) == 0)
        {
            val_map.insert({ name, ALObject::list_type{} });
        }

        ALObject::list_type sub_list;
        for (const auto *at = child->FirstAttribute(); at; at = at->Next())
        {
            sub_list.push_back(env::intern(":@"s += std::string(at->Name())));
            sub_list.push_back(make_string(std::string(at->Value())));
        }

        auto text = child->GetText();
        if (text)
        {
            sub_list.push_back(env::intern(":#text"));
            sub_list.push_back(make_string(text));
        }
        auto l = make_list(sub_list);
        l->set_prop("--dict--", Qt);
        val_map[name].push_back(std::move(l));
    }

    ALObject::list_type map_list;
    for (auto &[name, list] : val_map)
    {
        map_list.push_back(env::intern(name));

        if (list.size() == 1)
        {
            auto l = make_object(list[0]);
            l->set_prop("--dict--", Qt);
            map_list.push_back(std::move(l));
        }
        else
        {
            auto l = make_object(list);
            l->set_prop("--array--", Qt);
            map_list.push_back(std::move(l));
        }
    }

    auto res = make_list(map_list);
    res->set_prop("--dict--", Qt);
    return res;
}

ALObjectPtr xml_to_sexp(XMLDocument &t_doc)
{

    ALObject::list_type lis;

    for (const auto *child = t_doc.FirstChildElement(); child; child = child->NextSiblingElement())
    {
        auto name = ":"s += std::string{ child->Name() };

        auto text = child->GetText();
        if (text)
        {
            lis.push_back(env::intern(":#text"));
            lis.push_back(make_string(text));
        }

        auto child_obj = node_to_sexp(child);


        for (auto at = child->FirstAttribute(); at; at = at->Next())
        {
            auto at_name  = ":@"s += std::string(at->Name());
            auto at_value = std::string(at->Value());
            child_obj->children().push_back(env::intern(at_name));
            child_obj->children().push_back(make_string(at_value));
        }

        lis.push_back(env::intern(name));
        lis.push_back(child_obj);
    }

    auto res = make_list(lis);
    res->set_prop("--dict--", Qt);
    return res;
}

void sexp_to_node(ALObjectPtr t_obj, XMLDocument &t_doc, XMLNode *t_node, std::string t_key = {})
{

    if (t_obj->prop_exists("--dict--"))
    {

        for (size_t i = 0; i < t_obj->size() - 1; i += 2)
        {
            auto key = utility::erase_substr(t_obj->i(i)->to_string(), ":");
            auto obj = t_obj->i(i + 1);

            if (key[0] == '@')
            {
                t_node->ToElement()->SetAttribute(utility::erase_substr(key, "@").c_str(), obj->to_string().c_str());
            }
            else if (key[0] == '#')
            {

                t_node->InsertEndChild(t_doc.NewText(utility::erase_substr(obj->to_string(), "#").c_str()));
            }
            else
            {

                if (obj->prop_exists("--array--"))
                {
                    sexp_to_node(obj, t_doc, t_node, key);
                    continue;
                }
                auto xml_element = t_node->InsertEndChild(t_doc.NewElement(key.c_str()));
                sexp_to_node(obj, t_doc, xml_element);
            }
        }
    }
    else if (t_obj->prop_exists("--array--"))
    {

        for (auto &el : *t_obj)
        {

            auto xml_element = t_node->InsertEndChild(t_doc.NewElement(t_key.c_str()));
            sexp_to_node(el, t_doc, xml_element);
        }
    }
}

void sexp_to_xml(ALObjectPtr t_obj, XMLDocument &t_doc, XMLNode *t_node)
{

    if (t_obj->prop_exists("--dict--"))
    {

        for (size_t i = 0; i < t_obj->size() - 1; i += 2)
        {
            auto key = utility::erase_substr(t_obj->i(i)->to_string(), ":");
            auto obj = t_obj->i(i + 1);

            if (key[0] == '@')
            {
            }
            else if (key[0] == '#')
            {
            }
            else
            {
                auto xml_element = t_node->InsertEndChild(t_doc.NewElement(key.c_str()));
                sexp_to_node(obj, t_doc, xml_element);
            }
        }
    }
}

ALObjectPtr from_string(const std::string &t_input)
{
    XMLDocument doc;
    doc.Parse(t_input.c_str());
    return detail::xml_to_sexp(doc);
}

std::string to_string(ALObjectPtr t_xml)
{

    XMLDocument doc;
    detail::sexp_to_xml(t_xml, doc, &doc);

    XMLPrinter printer;
    doc.Print(&printer);

    return std::string(printer.CStr());
}


}  // namespace detail

struct parse_xml
{
    inline static const std::string name{"xml-parse"};

    inline static const Signature signature{String{}};

    inline static const std::string doc{R"((xml-parse STRING)

Parse a xml-formated string and return a alist representation of the xml)"};

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_size<1>(obj);
        auto str = eval->eval(obj->i(0));
        assert_string(str);
        return detail::from_string(str->to_string());
    }
};

struct dump_xml
{
    inline static const std::string name{"xml-dump"};

    inline static const Signature signature{Any{}};

    inline static const std::string doc{R"((xml-parse ALIST)
Convert a alist to a xml-formated string. Return the formated string.
)"};

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_size<1>(obj);
        auto xml = eval->eval(obj->i(0));

        return make_string(detail::to_string(xml));
    }
};

struct dump_file
{
    inline static const std::string name{"dump-file"};

    inline static const Signature signature{String{}, Any{}};

    inline static const std::string doc{R"((dump-file FILE ALIST)

Save the xml-formated string representation of `ALIST` in the file pointed by `PATH`.
)"};

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        assert_size<2>(obj);

        auto xml  = eval->eval(obj->i(0));
        auto file = eval->eval(obj->i(1));

        assert_string(file);

        // if (!fs::exists(file->to_string())) { return Qnil; }
        // if (!fs::is_regular_file(file->to_string())) { return Qnil; }

        std::ofstream outfile;
        outfile.open(file->to_string(), std::ios_base::out);
        if (outfile.is_open())
        {
            return Qnil;
        }
        outfile << detail::to_string(xml);

        return Qt;
    }
};

struct load_file
{
    inline static const std::string name{"load-file"};

    inline static const Signature signature{String{}};

    inline static const std::string doc{R"((load-file FILE)

Parse the contents of a file as xml and return a alist representation of the xml.
)"};

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;

        assert_size<1>(obj);
        auto file = eval->eval(obj->i(0));
        assert_string(file);

        if (!fs::exists(file->to_string()))
        {
            return Qnil;
        }
        if (!fs::is_regular_file(file->to_string()))
        {
            return Qnil;
        }

        return detail::from_string(utility::load_file(file->to_string()));
    }
};


}  // namespace xml

ALISP_EXPORT alisp::env::ModulePtr init_xml(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mxml    = alisp::module_init("xml");
    auto xml_ptr = Mxml.get();

    module_doc(xml_ptr,
               R"(The `xml` module enables the handling of XML-formated data. It
provides functionality for parsing and dumping s-expressions as XML.

Internally `xml` uses the
[tinyxml2](https://github.com/leethomason/tinyxml2) library.


)");

    return Mxml;
}
