#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_files.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


ALObjectPtr Ffile_open(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto name = eval->eval(t_obj->i(0));
    assert_string(name);

    

    auto file_obj = FileHelpers::open_file(name, Qnil, Qt);

    return file_obj;
}


ALObjectPtr Ffile_close(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto file = eval->eval(t_obj->i(0));
    assert_int(file);

    FileHelpers::close_file(file);
    return Qt;
}



ALObjectPtr Ffile_read_line(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    
    assert_size<1>(t_obj);
    auto file = eval->eval(t_obj->i(0));
    assert_int(file);

    auto& file_obj = FileHelpers::get_file(file);

    if (file_obj.m_input and !file_obj.m_file.eof()) {
        std::string line;
        std::getline(file_obj.m_file, line);
        return make_string(line);
    }

    return Qnil;
}


ALObjectPtr Ffile_write_line(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    
    assert_size<2>(t_obj);
    auto file = eval->eval(t_obj->i(0));
    assert_int(file);

    auto line = eval->eval(t_obj->i(1));
    assert_string(line);

    auto& file_obj = FileHelpers::get_file(file);

    if (file_obj.m_output) {
        file_obj.m_file << line;
        return Qt;
    }
    
    return Qnil;
}


ALObjectPtr Ffile_has_more(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    
    assert_size<1>(t_obj);
    auto file = eval->eval(t_obj->i(0));
    assert_int(file);

    auto& file_obj = FileHelpers::get_file(file);

    return !file_obj.m_file.eof() ? Qt : Qnil;
}

}
