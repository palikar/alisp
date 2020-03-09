#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/management/registry.hpp"

#include <tuple>
#include <utility>

#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wshadow"
#endif

#include <subprocess.hpp>

#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

namespace process
{
using namespace alisp;
using namespace subprocess;

namespace detail
{

static constexpr size_t BUFSIZE = 0;
static constexpr size_t DEFER = 1;
static constexpr size_t CLOSE_FDS = 2;
static constexpr size_t SESSION = 3;
static constexpr size_t CWD = 4;
static constexpr size_t ENV = 5;
static constexpr size_t INPUT = 6;
static constexpr size_t OUTPUT = 7;
static constexpr size_t ERROR = 8;
static constexpr size_t SHELL = 9;

using opts = std::tuple<bufsize, defer_spawn, close_fds, session_leader, cwd, environment, input, output, error, shell>;

// inline management::Registry<subprocess::Buffer, 0x05> buffer_registry;
inline management::Registry<subprocess::Popen, 0x06> proc_registry;

template<typename Args, typename Opts, size_t ... I>
inline auto open_proc(Args&& args, Opts&& options, std::index_sequence<I...>) {
    return subprocess::Popen(std::forward<Args>(args), std::move<decltype(std::get<I>(options))>(std::get<I>(options)) ... );
}

}

auto process_stdout = alisp::make_symbol("stdout");
auto process_stderr = alisp::make_symbol("stderr");
auto process_pipe = alisp::make_symbol("pipe");


ALObjectPtr Fpopen(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    auto l = eval->eval(obj->i(0));
    auto op = eval->eval(obj->i(1));

    AL_CHECK(assert_list(l));
    AL_CHECK(assert_list(op));

    std::vector<std::string> args;
    for (auto el : *l) {
        args.push_back(eval->eval(el)->to_string());
    }

    detail::opts options{};

    if (contains(op, ":defer")) {
        std::get<detail::DEFER>(options)  = defer_spawn{true};
    }

    if (auto [size, succ] = get_next(op, ":buff-size"); succ) {
        
        std::get<detail::BUFSIZE>(options) = bufsize{static_cast<int>(size->to_int())};
    }

    if (contains(op, ":close-fds")) {
        std::get<detail::CLOSE_FDS>(options) = close_fds{true};
    }

    if (auto [cwd_str, succ] = get_next(op, ":cwd"); succ) {
        std::get<detail::CWD>(options) = cwd{cwd_str->to_string()};
        
    }

    if (contains(op, ":shell")) {
        std::get<detail::SHELL>(options) = shell{true};
    }

    if (contains(op, ":env")) {

    }

    if (auto [in, succ] = get_next(op, ":input"); succ) {
        if (eq(in, process_stderr)) {
            std::get<detail::INPUT>(options) = input{STDERR};
        } else if (eq(in, process_stdout)) {
            std::get<detail::INPUT>(options) = input{STDOUT};
        } else if (eq(in, process_pipe)) {
            std::get<detail::INPUT>(options) = input{PIPE};
        }
    }

    if (auto [out, succ] = get_next(op, ":output"); succ) {
        if (eq(out, process_stderr)) {
            std::get<detail::OUTPUT>(options) = output{STDERR};
        } else if (eq(out, process_stdout)) {
            std::get<detail::OUTPUT>(options) = output{STDOUT};
        } else if (eq(out, process_pipe)) {
            std::get<detail::OUTPUT>(options) = output{PIPE};
        }
    }
    
    if (auto [err, succ] = get_next(op, ":error"); succ) {
        if (eq(err, process_stderr)) {
            std::get<detail::ERROR>(options) = error{STDERR};
        } else if (eq(err, process_stdout)) {
            std::get<detail::ERROR>(options) = error{STDOUT};
        } else if (eq(err, process_pipe)) {
            std::get<detail::ERROR>(options) = error{PIPE};
        }
    }

    auto new_id  = detail::proc_registry.put_resource(detail::open_proc(args,options, std::make_index_sequence<std::tuple_size<detail::opts>::value>() ))->id;
    auto new_obj = resource_to_object(new_id);
    return new_obj;
}

ALObjectPtr Fcheck_output(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    std::vector<std::string> args;
    for (auto el : *obj) {
        args.push_back(eval->eval(el)->to_string());
    }

    auto buf = subprocess::check_output(args);

    return make_string(std::string{buf.buf.begin(), buf.buf.end()});
}

ALObjectPtr Fcheck_output_bytes(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    std::vector<std::string> args;
    for (auto el : *obj) {
        args.push_back(eval->eval(el)->to_string());
    }

    auto buf = subprocess::check_output(args);

    ALObject::list_type bytes;
    for (auto b : buf.buf) { bytes.push_back(make_int(b)); }
    return make_list(bytes);
}

ALObjectPtr Fpid(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)].pid());
}

ALObjectPtr Fretcode(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)].retcode());
}

ALObjectPtr Fwait(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)].wait());
}

ALObjectPtr Fpoll(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)].poll());
}

ALObjectPtr Fstart(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    detail::proc_registry[object_to_resource(pro)].start_process();
    return Qt;
}

ALObjectPtr Fkill(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_min_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    if (std::size(*t_obj) > 1) {
        auto sig = eval->eval(t_obj->i(1));
        AL_CHECK(assert_int(sig));
        detail::proc_registry[object_to_resource(pro)].kill(static_cast<int>(sig->to_int()));
        return Qt;
    }

    detail::proc_registry[object_to_resource(pro)].kill();
    return Qt;
}

ALObjectPtr Fsend(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<2>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));
    auto msgs = eval->eval(t_obj->i(1));
    AL_CHECK(assert_string(msgs));

    auto s = msgs->to_string();
    return make_int(detail::proc_registry[object_to_resource(pro)].send(s.data(), s.size()));
}

ALObjectPtr Fcommunicate(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_min_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));
    
    if (std::size(*t_obj) > 1) {
        auto msgs = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(msgs));
        auto s = msgs->to_string();
        auto [out, err] = detail::proc_registry[object_to_resource(pro)].communicate(s.data(), s.size());
        return make_object(make_string(std::string{out.buf.begin(), out.buf.end()}),
                           make_string(std::string{err.buf.begin(), err.buf.end()}));
    }

    auto [out, err] = detail::proc_registry[object_to_resource(pro)].communicate();

    
    return make_object(make_string(std::string{out.buf.begin(), out.buf.end()}),
                       make_string(std::string{err.buf.begin(), err.buf.end()}));
}


}

ALISP_EXPORT alisp::env::ModulePtr init_process(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mprocess = alisp::module_init("process");
    auto prop_ptr = Mprocess.get();

    alisp::module_defconst(prop_ptr, "stdout", process::process_stdout);
    alisp::module_defconst(prop_ptr, "stderr", process::process_stderr);
    alisp::module_defconst(prop_ptr, "pipe", process::process_pipe);

    alisp::module_defun(prop_ptr, "popen", &process::Fpopen);
    alisp::module_defun(prop_ptr, "start", &process::Fstart);
    alisp::module_defun(prop_ptr, "pid", &process::Fpid);
    alisp::module_defun(prop_ptr, "wait", &process::Fwait);
    alisp::module_defun(prop_ptr, "poll", &process::Fpoll);
    alisp::module_defun(prop_ptr, "kill", &process::Fkill);

    alisp::module_defun(prop_ptr, "send", &process::Fsend);
    alisp::module_defun(prop_ptr, "communicate", &process::Fcommunicate);
    
    alisp::module_defun(prop_ptr, "check-output", &process::Fcheck_output);
    alisp::module_defun(prop_ptr, "check-output-bytes", &process::Fcheck_output_bytes);

    alisp::module_defun(prop_ptr, "call", &process::Fcheck_output);

    alisp::module_defun(prop_ptr, "pipeline", &process::Fcheck_output);

    return Mprocess;
}
