#include <algorithm>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_streams.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


ALObjectPtr Fstream(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    auto [string_sym, from_string] = get_next(t_obj, ":from-string");
    auto [file_sym, from_file] = get_next(t_obj, ":from-file");

    
    if (from_string) {
        auto obj = eval->eval(string_sym);
        assert_string(obj);
        return StreamsHelper::create_string_stream(obj);
    }

    if (from_file) {
        auto obj = eval->eval(file_sym);
        assert_int(obj);
        return StreamsHelper::create_file_stream(obj);
    
    }

    return Qnil;
}

ALObjectPtr Fclose_stream(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);
    StreamsHelper::close_stream(obj);
    return Qt;
}

ALObjectPtr Fwith_cout(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    StreamsHelper::rebind_cout(obj);

    CoutRestore cout;

    return eval_list(eval, t_obj, 1);
}

ALObjectPtr Fwith_cin(ALObjectPtr, env::Environment *, eval::Evaluator *)
{


    return Qnil;
}

ALObjectPtr Fstream_content(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);
    auto stream = StreamsHelper::get_stream(obj);
    return make_string(stream->content());
}

ALObjectPtr Fstream_write(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    auto str_obj = eval->eval(t_obj->i(1));
    assert_string(str_obj);

    auto stream = StreamsHelper::get_stream(obj);
    (*stream) << str_obj->to_string();
    return Qt;
}

ALObjectPtr Fstream_write_line(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    auto str_obj = eval->eval(t_obj->i(1));
    assert_string(str_obj);

    auto stream = StreamsHelper::get_stream(obj);
    (*stream) << str_obj->to_string() << '\n';
    return Qt;
}

ALObjectPtr Fstream_write_lines(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    auto stream = StreamsHelper::get_stream(obj);

    auto str_objs = eval->eval(t_obj->i(1));
    assert_list(str_objs);
    auto eval_objs = eval_transform(eval, str_objs);
    for (auto &el : *eval_objs) { (*stream) << el->to_string() << '\n'; }
    return Qt;
}

ALObjectPtr Fstream_read(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    auto int_obj = eval->eval(t_obj->i(1));
    assert_int(int_obj);

    auto stream  = StreamsHelper::get_stream(obj);
    auto content = (*stream).get_chars(static_cast<size_t>(int_obj->to_int()));
    return make_string(content);
}

ALObjectPtr Fstream_read_line(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    auto stream  = StreamsHelper::get_stream(obj);
    auto content = (*stream).get_line();
    return make_string(content);
}

ALObjectPtr Fstream_read_lines(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    assert_size<1>(t_obj);
    auto obj = eval->eval(t_obj->i(0));
    assert_int(obj);

    auto stream = StreamsHelper::get_stream(obj);

    ALObject::list_type lines{};
    while (stream->hasmore()) { lines.push_back(make_string((*stream).get_line())); }

    return make_object(lines);
}

}  // namespace alisp
