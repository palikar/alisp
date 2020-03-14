#include "alisp/config.hpp"

#include "alisp/alisp/alisp_module_helpers.hpp"
#include "alisp/alisp/alisp_env.hpp"


namespace func
{
using namespace alisp;

namespace details
{

}


ALObjectPtr Fpartial(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(obj));

    auto fun = obj->i(0);

    // (partial '+ 1)
    // (lambda (&rest a) `(apply + ,@rest))

    // (partial '+ _ 1)
    // (lambda (a &rest b) (+ a 1 rest))

    return Flambda(
      make_object(make_object(Qrest, make_symbol("rest--")),
                  make_object(make_symbol("eval"),
                              make_object(make_symbol("backquote"),
                                          make_object(fun, obj->i(1), make_object(Qcomma_at, make_symbol("rest--")))))),
      env,
      eval);
}

ALObjectPtr Fthread_first(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<2>(obj);

    auto first = obj->i(0);

    const size_t size = std::size(*obj);

    for (size_t i = 1; i < size; ++i)
    {
        auto next_ls = obj->i(i);
        ALObject::list_type l;
        l.push_back(next_ls->i(0));
        l.push_back(first);
        for (size_t j = 1; j < std::size(*next_ls); ++j) { l.push_back(next_ls->i(j)); }
        first = make_list(l);
    }

    return eval->eval(first);
}

ALObjectPtr Fthread_last(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_min_size<2>(obj);

    auto first = obj->i(0);

    const size_t size = std::size(*obj);

    for (size_t i = 1; i < size; ++i)
    {
        auto next_ls = obj->i(i);
        ALObject::list_type l;
        l.push_back(next_ls->i(0));
        for (size_t j = 1; j < std::size(*next_ls); ++j) { l.push_back(next_ls->i(j)); }
        l.push_back(first);
        first = make_list(l);
    }
    return eval->eval(first);
}

ALObjectPtr Freduce(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));

    auto fun = eval->eval(obj->i(0));
    AL_CHECK(assert_function(fun));

    auto list = eval->eval(obj->i(1));
    AL_CHECK(assert_list(list));

    const size_t size = std::size(*list);

    if (size < 2) { return Qnil; }

    auto res = list->i(0);
    for (size_t i = 1; i < size; ++i) { res = eval->handle_lambda(fun, make_object(res, list->i(i))); }

    return res;
}

ALObjectPtr Fidentity(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<1>(obj));
    return obj->i(0);
}

ALObjectPtr Fignore(ALObjectPtr, env::Environment *, eval::Evaluator *)
{
    return Qnil;
}

}  // namespace func

ALISP_EXPORT alisp::env::ModulePtr init_func(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mfunc   = alisp::module_init("func");
    auto fun_ptr = Mfunc.get();

    alisp::module_doc(fun_ptr, R"()");

    alisp::module_defvar(fun_ptr, "_", alisp::make_symbol("_"));

    alisp::module_defun(fun_ptr, "partial", &func::Fpartial);
    alisp::module_defun(fun_ptr, "thread-last", &func::Fthread_last);
    alisp::module_defun(fun_ptr, "thread-first", &func::Fthread_first);
    alisp::module_defun(fun_ptr, "reduce", &func::Freduce);
    alisp::module_defun(fun_ptr, "identity", &func::Fidentity);
    alisp::module_defun(fun_ptr, "ignore", &func::Fignore);

    return Mfunc;
}
