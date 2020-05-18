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

#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/management/registry.hpp"

#include <tuple>
#include <utility>
#include <memory>

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

static constexpr size_t BUFSIZE   = 0;
static constexpr size_t DEFER     = 1;
static constexpr size_t CLOSE_FDS = 2;
static constexpr size_t SESSION   = 3;
static constexpr size_t CWD       = 4;
static constexpr size_t ENV       = 5;
static constexpr size_t INPUT     = 6;
static constexpr size_t OUTPUT    = 7;
static constexpr size_t ERROR     = 8;
static constexpr size_t SHELL     = 9;

using opts = std::tuple<bufsize, defer_spawn, close_fds, session_leader, cwd, environment, input, output, error, shell>;

inline management::Registry<std::unique_ptr<subprocess::Popen>, 0x06> proc_registry;

template<typename Args, typename Opts, size_t... I>
inline auto open_proc(Args &&args, Opts &&options, std::index_sequence<I...>)
{
    return std::make_unique<subprocess::Popen>(std::forward<Args>(args),
                                               std::move<decltype(std::get<I>(options))>(std::get<I>(options))...);
}

}  // namespace detail

auto process_stdout = alisp::make_symbol("stdout");
auto process_stderr = alisp::make_symbol("stderr");
auto process_pipe   = alisp::make_symbol("pipe");


ALObjectPtr Fpopen(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    auto l  = eval->eval(obj->i(0));
    auto op = eval->eval(obj->i(1));

    AL_CHECK(assert_list(l));
    AL_CHECK(assert_list(op));

    std::vector<std::string> args;
    for (auto el : *l)
    {
        args.push_back(eval->eval(el)->to_string());
    }

    detail::opts options{};

    if (contains(op, ":defer"))
    {
        std::get<detail::DEFER>(options) = defer_spawn{ true };
    }

    if (auto [size, succ] = get_next(op, ":buff-size"); succ)
    {
        auto s = eval->eval(size);
        AL_CHECK(assert_int(s));
        std::get<detail::BUFSIZE>(options) = bufsize{ static_cast<int>(s->to_int()) };
    }

    if (contains(op, ":close-fds"))
    {
        std::get<detail::CLOSE_FDS>(options) = close_fds{ true };
    }

    if (auto [cwd_str, succ] = get_next(op, ":cwd"); succ)
    {
        auto s = eval->eval(cwd_str);
        AL_CHECK(assert_string(s));
        std::get<detail::CWD>(options) = cwd{ s->to_string() };
    }

    if (contains(op, ":shell"))
    {
        std::get<detail::SHELL>(options) = shell{ true };
    }

    if (contains(op, ":env"))
    {
    }

    if (auto [in, succ] = get_next(op, ":input"); succ)
    {
        if (eq(eval->eval(in), process_stderr))
        {
            std::get<detail::INPUT>(options) = input{ STDERR };
        }
        else if (eq(eval->eval(in), process_stdout))
        {
            std::get<detail::INPUT>(options) = input{ STDOUT };
        }
        else if (eq(eval->eval(in), process_pipe))
        {
            std::get<detail::INPUT>(options) = input{ PIPE };
        }
    }

    if (auto [out, succ] = get_next(op, ":output"); succ)
    {

        if (eq(eval->eval(out), process_stderr))
        {
            std::get<detail::OUTPUT>(options) = output{ STDERR };
        }
        else if (eq(eval->eval(out), process_stdout))
        {
            std::get<detail::OUTPUT>(options) = output{ STDOUT };
        }
        else if (eq(eval->eval(out), process_pipe))
        {
            std::get<detail::OUTPUT>(options) = output{ PIPE };
        }
    }

    if (auto [err, succ] = get_next(op, ":error"); succ)
    {
        if (eq(eval->eval(err), process_stderr))
        {
            std::get<detail::ERROR>(options) = error{ STDERR };
        }
        else if (eq(eval->eval(err), process_stdout))
        {
            std::get<detail::ERROR>(options) = error{ STDOUT };
        }
        else if (eq(eval->eval(err), process_pipe))
        {
            std::get<detail::ERROR>(options) = error{ PIPE };
        }
    }

    auto new_id = detail::proc_registry
                    .emplace_resource(detail::open_proc(
                      args, options, std::make_index_sequence<std::tuple_size<detail::opts>::value>()))
                    ->id;

    env->defer_callback([id = new_id]() { detail::proc_registry.destroy_resource(id); });

    auto new_obj = resource_to_object(new_id);
    return new_obj;
}

ALObjectPtr Fcheck_output(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    std::vector<std::string> args;
    for (auto el : *obj)
    {
        args.push_back(eval->eval(el)->to_string());
    }

    auto buf = subprocess::check_output(args);

    return make_string(std::string{ buf.buf.begin(), buf.buf.end() });
}

ALObjectPtr Fcheck_output_bytes(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    std::vector<std::string> args;
    for (auto el : *obj)
    {
        args.push_back(eval->eval(el)->to_string());
    }

    auto buf = subprocess::check_output(args);

    ALObject::list_type bytes;
    for (auto b : buf.buf)
    {
        bytes.push_back(make_int(b));
    }
    return make_list(bytes);
}

ALObjectPtr Fpid(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)]->pid());
}

ALObjectPtr Fretcode(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)]->retcode());
}

ALObjectPtr Fwait(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)]->wait());
}

ALObjectPtr Fpoll(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    return make_int(detail::proc_registry[object_to_resource(pro)]->poll());
}

ALObjectPtr Fstart(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    detail::proc_registry[object_to_resource(pro)]->start_process();
    return Qt;
}

ALObjectPtr Fkill(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_min_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    if (std::size(*t_obj) > 1)
    {
        auto sig = eval->eval(t_obj->i(1));
        AL_CHECK(assert_int(sig));
        detail::proc_registry[object_to_resource(pro)]->kill(static_cast<int>(sig->to_int()));
        return Qt;
    }

    detail::proc_registry[object_to_resource(pro)]->kill();
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
    return make_int(detail::proc_registry[object_to_resource(pro)]->send(s.data(), s.size()));
}

ALObjectPtr Fcommunicate(ALObjectPtr t_obj, env::Environment *, eval::Evaluator *eval)
{

    AL_CHECK(assert_min_size<1>(t_obj));
    auto pro = eval->eval(t_obj->i(0));
    AL_CHECK(assert_int(pro));

    if (std::size(*t_obj) > 1)
    {
        auto msgs = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(msgs));
        auto s          = msgs->to_string();
        auto [out, err] = detail::proc_registry[object_to_resource(pro)]->communicate(s.data(), s.size());
        return make_object(make_string(std::string{ out.buf.begin(), out.buf.end() }),
                           make_string(std::string{ err.buf.begin(), err.buf.end() }));
    }

    auto [out, err] = detail::proc_registry[object_to_resource(pro)]->communicate();


    return make_object(make_string(std::string{ out.buf.begin(), out.buf.end() }),
                       make_string(std::string{ err.buf.begin(), err.buf.end() }));
}


}  // namespace process

ALISP_EXPORT alisp::env::ModulePtr init_process(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mprocess = alisp::module_init("process");
    auto prop_ptr = Mprocess.get();

    alisp::module_doc(
      prop_ptr,
      R"(The `process` module enables the starting and communicating with
external processes. It is similar to the `subprocess` module of
pyhton. The module tries, in fact, to stay close the the api and
privide similar functions for starting and communicating with external
processes.

Internaly `process` uses the [cpp-subprocess](https://github.com/arun11299/cpp-subprocess)
library.

)");

    alisp::module_defconst(prop_ptr, "stdout", process::process_stdout,
    R"(Symbol used to signify the standard output stream. It is used in some of the functions of the module. )");

    alisp::module_defconst(prop_ptr, "stderr", process::process_stderr,
    R"(Symbol used to signify the standard input stream. It is used in some of the functions of the module. )");

    alisp::module_defconst(prop_ptr, "pipe", process::process_pipe,
    R"(Symbol used to signify a link between the spawn process and the interpreter. It is used in some of the functions of the module. )");

    alisp::module_defun(prop_ptr, "popen", &process::Fpopen,
    R"((open COMMAND_PARTS OPTIONS )

Execute a process. `COMMAND_PARTS` must be a list of strings that will
become the parts of the command that should be executed.

`OPTIONS` is also a list with options on how to execute the
process. Possible options are:

  * `:defer` - if present, don't start the process immediately but only when the `start` function is called.
  * `:buff-size` - the buffer size of the stdin/stdout/stderr streams of the child process. Default value is 0.
  * `:close-fds` - if present, close all file descriptors when the child process is spawned.
  * `:cwd` - the working directory where the process should be executed.
  * `:shell` - if present, spawn the process in a sub-shell.
  * `:env` - a list of pairs `(VAR VALUE)`. For the spawned process, the env variable `VAR` will be set to `VALUE`.
  * `:input` - specify the input channel fot the child process. This can be `pipe`\`stdout`\`stderr` or a file name.
  * `:output` - specify the output channel fot the child process. This can be `pipe`\`stdout`\`stderr` or a file name.
  * `:error` - specify the error channel fot the child process. This can be `pipe`\`stdout`\`stderr` or a file name.

Return the new process as a resource object.
)");

    alisp::module_defun(prop_ptr, "start", &process::Fstart,
    R"((start PROCESS)

Start a process that has been created with `open`.
)");

    alisp::module_defun(prop_ptr, "pid", &process::Fpid,
    R"((pid PROCESS)

Return the process id of a process that has been created with `open`.
)");

    alisp::module_defun(prop_ptr, "wait", &process::Fwait,
    R"((wait PROCESS)

Block until a process has finished its execution.
)");

    alisp::module_defun(prop_ptr, "poll", &process::Fpoll,
    R"((poll PROCESS)

)");

    alisp::module_defun(prop_ptr, "kill", &process::Fkill,
    R"((kill PROCESS [SIGNAL])

Send a signal (by default SIGKILL) to a running process.
)");

    alisp::module_defun(prop_ptr, "retcode", &process::Fretcode,
    R"((retcode PROCESS)

Wait for a process to finish and return its return code.
)");



    alisp::module_defun(prop_ptr, "send", &process::Fsend,
    R"((send PROCESS STRING)

Write a string to the standard input stream of a child process.
)");

    alisp::module_defun(prop_ptr, "communicate", &process::Fcommunicate,
    R"((communicate PROCESS STRING)

Write a string to the standard input stream of a child process. Return
the contents of the standard output and standard error of the process.
)");

    alisp::module_defun(prop_ptr, "check-output", &process::Fcheck_output,
    R"((check-output [COMMAND_PART]...)

Convenience function. Execute the command with the given parts and
return the contents of the standard output of the process once its
finished.
)");

    alisp::module_defun(prop_ptr, "check-output-bytes", &process::Fcheck_output_bytes,
    R"((check-output [COMMAND_PART]...)

Convenience function. Execute the command with the given parts and
return the contents of the standard output as a byte array.
)");

    alisp::module_defun(prop_ptr, "call", &process::Fcheck_output,
    R"((check-output [COMMAND_PART]...)

Convenience function. Execute the command with the given parts and
return the exit code of the process once its finished.
)");

    // alisp::module_defun(prop_ptr, "pipeline", &process::Fcheck_output);

    return Mprocess;
}
