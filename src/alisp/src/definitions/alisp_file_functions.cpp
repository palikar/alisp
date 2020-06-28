#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_files.hpp"

#include "alisp/utility/macros.hpp"

#include "alisp/alisp/declarations/files.hpp"

namespace alisp
{


ALObjectPtr Ffile_open(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(t_obj));
    auto name = eval_check(eval, t_obj, 0, &assert_string<size_t>);


    auto output = contains(t_obj, ":out") ? Qt : Qnil;
    auto input  = contains(t_obj, ":in") ? Qt : Qnil;

    auto file_obj = FileHelpers::open_file(name, output, input);

    return file_obj;
}


ALObjectPtr Ffile_close(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(t_obj));
    auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);

    FileHelpers::close_file(file);
    return Qt;
}


ALObjectPtr Ffile_read_line(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);

    auto &file_obj = FileHelpers::get_file(file);

    if (file_obj.m_input and !file_obj.m_file.eof())
    {
        std::string line;
        std::getline(file_obj.m_file, line);
        return make_string(line);
    }

    return Qnil;
}


ALObjectPtr Ffile_write_line(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<2>(t_obj));
    auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);
    auto line = eval_check(eval, t_obj, 1, &assert_string<size_t>);

    auto &file_obj = FileHelpers::get_file(file);

    if (file_obj.m_output)
    {
        file_obj.m_file << line->to_string() << '\n';
        return Qt;
    }

    return Qnil;
}


ALObjectPtr Ffile_has_more(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));

    auto file = eval_check(eval, t_obj, 0, &assert_file<size_t>);

    auto &file_obj = FileHelpers::get_file(file);

    return !file_obj.m_file.eof() ? Qt : Qnil;
}

}  // namespace alisp
