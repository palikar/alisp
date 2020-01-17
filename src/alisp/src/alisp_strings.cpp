#include <algorithm>
#include <string>
#include <cctype>

#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_env.hpp"

#include "alisp/utility/string_utils.hpp"

namespace alisp
{

ALObjectPtr Fstring_contains(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);
    
    if (str_1->to_string().find(str_2->to_string()) != std::string::npos) {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_endswith(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);
    
    if (utility::ends_with(str_1->to_string(), str_2->to_string())) {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_startswtih(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);
    
    if (utility::starts_with(str_1->to_string(), str_2->to_string())) {
        return Qt;
    }
    return Qnil;
}

ALObjectPtr Fstring_length(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_string(str);
    return make_int(std::size(str->to_string()));
}

ALObjectPtr Fstring_capitalize(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_string(str);
    auto& s = str->to_string();
    s[0] = static_cast<char>(std::toupper(static_cast<int>(s[0])));
    return str;
}

ALObjectPtr Fstring_isalpha(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_char(str);
    return std::isalpha(static_cast<int>(str->to_int())) ? Qt : Qnil;
}

ALObjectPtr Fstring_isdigit(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str = eval->eval(obj->i(0));
    assert_char(str);
    return std::isdigit(static_cast<int>(str->to_int())) ? Qt : Qnil;
}

ALObjectPtr Fstring_find(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);

    auto pos = str_1->to_string().find(str_2->to_string());
    if (pos != std::string::npos) {
        return make_int(pos);
    }
    return Qnil;
}

ALObjectPtr Fstring_replace(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<3>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    auto str_3 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);
    assert_string(str_3);
    return make_string(utility::replace(str_1->to_string(),str_2->to_string(),str_3->to_string()));
}

ALObjectPtr Fstring_replaceall(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<3>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    auto str_3 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);
    assert_string(str_3);

    return make_string(utility::replace_all(str_1->to_string(),str_2->to_string(),str_3->to_string()));
}

ALObjectPtr Fstring_split(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    auto str_1 = eval->eval(obj->i(0));
    auto str_2 = eval->eval(obj->i(1));
    assert_string(str_1);
    assert_string(str_2);

    auto tokens = utility::split(str_1->to_string(), str_2->to_string());

    ALObject::list_type token_objs(std::size(tokens));
    std::transform(std::begin(tokens), std::end(tokens), std::back_inserter(token_objs), make_string);
    
    return make_object(token_objs);
}

ALObjectPtr Fstring_substring(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<3>(obj);
    auto str = eval->eval(obj->i(0));
    auto ind_1 = eval->eval(obj->i(1));
    auto ind_2 = eval->eval(obj->i(2));
    assert_string(str);
    assert_int(ind_1);
    assert_int(ind_2);
    return make_string(
        str->to_string().substr(static_cast<ALObject::string_type::size_type>(ind_1->to_int()),
                                static_cast<ALObject::string_type::size_type>(ind_2->to_int())));
}

ALObjectPtr Fstring_splitlines(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str_1 = eval->eval(obj->i(0));
    assert_string(str_1);
    
    auto tokens = utility::split(str_1->to_string(), '\n');

    ALObject::list_type token_objs(std::size(tokens));
    std::transform(std::begin(tokens), std::end(tokens), std::back_inserter(token_objs), make_string);
    
    return make_object(token_objs);
}

ALObjectPtr Fstring_upper(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str_1 = eval->eval(obj->i(0));
    assert_string(str_1);
    return make_string(utility::str_toupper(str_1->to_string()));
}

ALObjectPtr Fstring_lower(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str_1 = eval->eval(obj->i(0));
    assert_string(str_1);
    return make_string(utility::str_tolower(str_1->to_string()));
}

ALObjectPtr Fstring_strip(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    auto str_1 = eval->eval(obj->i(0));
    assert_string(str_1);
    return make_string(utility::trim(str_1->to_string()));
}

ALObjectPtr Fstring_join(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(obj);
    std::string new_string{""};
    for (auto& t_obj : *obj) {
        auto e = eval->eval(t_obj);
        assert_string(e);
        new_string += e->to_string();
    }    
    return make_string(new_string);
}

}
