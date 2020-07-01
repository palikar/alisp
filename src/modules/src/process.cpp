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

auto subprocess_signal = alisp::make_symbol("subprocess-signal");

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


struct popen
{
    inline static const std::string name{ "popen" };

    inline static const Signature signature{ List{}, List{} };

    inline static const std::string doc{ R"(((open COMMAND_PARTS OPTIONS )

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
))" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
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

        try
        {
            auto new_id = detail::proc_registry
                            .emplace_resource(detail::open_proc(
                              args, options, std::make_index_sequence<std::tuple_size<detail::opts>::value>()))
                            ->id;
            env->defer_callback([id = new_id]() { detail::proc_registry.destroy_resource(id); });
            auto new_obj = resource_to_object(new_id);
            return new_obj;
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct check_output
{
    inline static const std::string name{ "check-output" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((check-output [COMMAND_PART]...)

Convenience function. Execute the command with the given parts and
return the contents of the standard output of the process once its
finished.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_min_size<1>(obj);

        std::vector<std::string> args;
        for (auto el : *obj)
        {
            args.push_back(eval->eval(el)->to_string());
        }

        try
        {
            auto buf = subprocess::check_output(args);
            return make_string(std::string{ buf.buf.begin(), buf.buf.end() });
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct check_output_bytes
{
    inline static const std::string name{ "check-output-bytes" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((check-output [COMMAND_PART]...)

Convenience function. Execute the command with the given parts and
return the contents of the standard output as a byte array.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_min_size<1>(obj);

        std::vector<std::string> args;
        for (auto el : *obj)
        {
            args.push_back(eval->eval(el)->to_string());
        }

        try
        {
            auto buf = subprocess::check_output(args);
            ALObject::list_type bytes;
            for (auto b : buf.buf)
            {
                bytes.push_back(make_int(b));
            }
            return make_list(bytes);
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct pid
{
    inline static const std::string name{ "pid" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((pid PROCESS)

Return the process id of a process that has been created with `open`.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        return make_int(detail::proc_registry[object_to_resource(pro)]->pid());
    }
};

struct retcode
{
    inline static const std::string name{ "retcode" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((retcode PROCESS)

Wait for a process to finish and return its return code.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        try
        {
            return make_int(detail::proc_registry[object_to_resource(pro)]->retcode());
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct wait
{
    inline static const std::string name{ "wait" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((wait PROCESS)

Block until a process has finished its execution.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        try
        {
            return make_int(detail::proc_registry[object_to_resource(pro)]->wait());
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct poll
{
    inline static const std::string name{ "poll" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"((poll PROCESS)

)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        try
        {
            return make_int(detail::proc_registry[object_to_resource(pro)]->poll());
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct start
{
    inline static const std::string name{ "start" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((start PROCESS)

Start a process that has been created with `open`.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        try
        {
            detail::proc_registry[object_to_resource(pro)]->start_process();
            return Qt;
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct kill
{
    inline static const std::string name{ "kill" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((kill PROCESS [SIGNAL])

Send a signal (by default SIGKILL) to a running process.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_min_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        try
        {
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
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct send
{
    inline static const std::string name{ "send" };

    inline static const Signature signature{ Int{}, String{} };

    inline static const std::string doc{ R"((send PROCESS STRING)

Write a string to the standard input stream of a child process.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_size<2>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));
        auto msgs = eval->eval(t_obj->i(1));
        AL_CHECK(assert_string(msgs));

        try
        {
            auto s = msgs->to_string();
            return make_int(detail::proc_registry[object_to_resource(pro)]->send(s.data(), s.size()));
        }
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct communicate
{
    inline static const std::string name{ "communicate" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"((communicate PROCESS STRING)

Write a string to the standard input stream of a child process. Return
the contents of the standard output and standard error of the process.
)" };

    static ALObjectPtr func(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {

        AL_CHECK(assert_min_size<1>(t_obj));
        auto pro = eval->eval(t_obj->i(0));
        AL_CHECK(assert_int(pro));

        try
        {

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
        catch (subprocess::OSError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: OSError: {}", exc.what()));
            return Qnil;
        }
        catch (subprocess::CalledProcessError &exc)
        {
            signal(subprocess_signal, fmt::format("Subprocess error: CalledProcessError: {}", exc.what()));
            return Qnil;
        }
    }
};

struct call
{
    inline static const std::string name{ "call" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"((call [COMMAND_PART]...)

    Convenience function. Execute the command with the given parts and
    return the exit code of the process once its finished.
    )" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_min_size<1>(obj);

        auto l = eval->eval(obj->i(0));

        AL_CHECK(assert_list(l));

        std::vector<std::string> args;
        for (auto el : *l)
        {
            args.push_back(eval->eval(el)->to_string());
        }

        detail::opts options{};
        auto ret = subprocess::Popen(args).wait();
        return make_int(ret);
    }
};

struct stdout_const
{

    inline static const std::string name = "stdout";

    inline static const std::string doc{
        R"(Symbol used to signify the standard output stream. It is used in some of the functions of the module. )"
    };

    inline static const auto var = process_stdout;
};

struct stderr_const
{

    inline static const std::string name = "stderr";

    inline static const std::string doc{
        R"(Symbol used to signify the standard input stream. It is used in some of the functions of the module. )"
    };

    inline static const auto var = process_stderr;
};

struct pipe_const
{

    inline static const std::string name = "pipe";

    inline static const std::string doc{
        R"(Symbol used to signify a link between the spawn process and the interpreter. It is used in some of the functions of the module. )"
    };

    inline static const auto var = process_pipe;
};

struct module_doc
{

    inline static const std::string doc{ R"(The `process` module enables the starting and communicating with
external processes. It is similar to the `subprocess` module of
pyhton. The module tries, in fact, to stay close the the api and
privide similar functions for starting and communicating with external
processes.

Internaly `process` uses the [cpp-subprocess](https://github.com/arun11299/cpp-subprocess)
library.

)" };
};

}  // namespace process

ALISP_EXPORT alisp::env::ModulePtr init_process(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mprocess = alisp::module_init("process");
    auto prop_ptr = Mprocess.get();

    module_doc(prop_ptr, process::module_doc::doc);

    module_defconst(prop_ptr, process::stdout_const::name, process::stdout_const::var, process::stdout_const::doc);
    module_defconst(prop_ptr, process::stderr_const::name, process::stderr_const::var, process::stderr_const::doc);
    module_defconst(prop_ptr, process::pipe_const::name, process::pipe_const::var, process::pipe_const::doc);

    module_defun(
      prop_ptr, process::popen::name, process::popen::func, process::popen::doc, process::popen::signature.al());
    module_defun(prop_ptr,
                 process::check_output::name,
                 process::check_output::func,
                 process::check_output::doc,
                 process::check_output::signature.al());
    module_defun(prop_ptr,
                 process::check_output_bytes::name,
                 process::check_output_bytes::func,
                 process::check_output_bytes::doc,
                 process::check_output_bytes::signature.al());
    module_defun(prop_ptr, process::pid::name, process::pid::func, process::pid::doc, process::pid::signature.al());
    module_defun(prop_ptr,
                 process::retcode::name,
                 process::retcode::func,
                 process::retcode::doc,
                 process::retcode::signature.al());
    module_defun(prop_ptr, process::wait::name, process::wait::func, process::wait::doc, process::wait::signature.al());
    module_defun(prop_ptr, process::poll::name, process::poll::func, process::poll::doc, process::poll::signature.al());
    module_defun(
      prop_ptr, process::start::name, process::start::func, process::start::doc, process::start::signature.al());
    module_defun(prop_ptr, process::kill::name, process::kill::func, process::kill::doc, process::kill::signature.al());
    module_defun(prop_ptr, process::send::name, process::send::func, process::send::doc, process::send::signature.al());
    module_defun(prop_ptr,
                 process::communicate::name,
                 process::communicate::func,
                 process::communicate::doc,
                 process::communicate::signature.al());
    module_defun(prop_ptr, process::call::name, process::call::func, process::call::doc, process::call::signature.al());


    return Mprocess;
}
