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

using opts = std::tuple<bufsize, defer_spawn, close_fds, session_leader, cwd, environment, input, output, error>;

// inline management::Registry<subprocess::Buffer, 0x05> buffer_registry;
inline management::Registry<subprocess::Popen, 0x06> proc_registry;

template<typename Args, typename Opts, size_t ... I>
inline auto open_proc(Args&& args, Opts&& options, std::index_sequence<I...>) {
    return subprocess::Popen(std::forward<Args>(args), std::move<decltype(std::get<I>(options))>(std::get<I>(options)) ... );

}

}

ALObjectPtr Fpopen(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<1>(obj);

    std::vector<std::string> args;
    for (auto el : *obj) {
        args.push_back(eval->eval(el)->to_string());
    }


    detail::opts options{};
    
    std::get<1>(options) = defer_spawn{true};

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

}

ALISP_EXPORT alisp::env::ModulePtr init_process(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mprocess = alisp::module_init("process");
    auto prop_ptr = Mprocess.get();

    alisp::module_defun(prop_ptr, "popen", &process::Fpopen);
    
    alisp::module_defun(prop_ptr, "check-output", &process::Fcheck_output);
    alisp::module_defun(prop_ptr, "check-output-bytes", &process::Fcheck_output_bytes);

    return Mprocess;
}
