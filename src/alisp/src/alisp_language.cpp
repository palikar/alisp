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


#include <algorithm>
#include <filesystem>
#include <string>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/alisp/alisp_loadable_modules.hpp"

#include "alisp/utility/macros.hpp"
#include "alisp/utility/hash.hpp"


namespace alisp
{

namespace detail
{

static ALObjectPtr handle_backquote_list(ALObjectPtr obj, eval::Evaluator *eval)
{
    if (not plist(obj)) { return obj; }

    if (obj->i(0) == Qcomma) { return eval->eval(obj->i(1)); }

    ALObject::list_type new_elements;


    for (auto el : *obj)
    {

        if (!plist(el))
        {
            new_elements.push_back(el);
            continue;
        }

        if (el->i(0) == Qcomma_at)
        {
            auto new_list = eval->eval(el->i(1));
            if (plist(new_list))
            {
                new_elements.insert(
                  new_elements.end(), std::begin(new_list->children()), std::end(new_list->children()));
            }
            else
            {
                new_elements.push_back(new_list);
            }
            continue;
        }

        new_elements.push_back(handle_backquote_list(el, eval));
    }

    return make_object(new_elements);
}

static bool check_arg_list(ALObjectPtr t_list)
{
    if (t_list == Qnil) { return true; }

    for (auto &el : *t_list)
    {
        if (!psym(el)) { return false; }
    }


    if (std::count(std::begin(*t_list), std::end(*t_list), Qrest) > 1) { return false; }


    if (std::count(std::begin(*t_list), std::end(*t_list), Qoptional) > 1) { return false; }

    auto rest_pos = std::find(std::begin(*t_list), std::end(*t_list), Qrest);
    auto opt_pos  = std::find(std::begin(*t_list), std::end(*t_list), Qoptional);
    if (rest_pos != std::end(*t_list) and opt_pos != std::end(*t_list) and rest_pos < opt_pos) { return false; }


    return true;
}

static ALObjectPtr catch_case_lippincott(ALObjectPtr t_obj, eval::Evaluator *eval)
{

#define HANDLE                                                                                   \
    for (size_t i = 2; i < t_obj->size(); ++i)                                                   \
    {                                                                                            \
        auto handler = t_obj->i(i);                                                              \
        auto sym     = eval->eval(handler->i(0));                                                \
        if (sym->to_string().compare(p_exc.name()) == 0) { return eval_list(eval, handler, 1); } \
    }                                                                                            \
    throw

    try
    {
        throw;
    }
    catch (environment_error &p_exc)
    {
        HANDLE;
    }
    catch (eval_error &p_exc)
    {
        HANDLE;
    }
    catch (signal_exception &p_exc)
    {
        HANDLE;
    }
    catch (argument_error &p_exc)
    {
        HANDLE;
    }
    catch (module_error &p_exc)
    {
        HANDLE;
    }
    catch (module_refence_error &p_exc)
    {
        HANDLE;
    }
    catch (interrupt_error &p_exc)
    {
        HANDLE;
    }
    catch (...)
    {
        throw;
    }

#undef HANDLE

    return Qt;
}

}  // namespace detail

ALObjectPtr Fimport(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    namespace fs = std::filesystem;
    AL_CHECK(assert_min_size<1>(obj));

    auto mod_sym = eval->eval(obj->i(0));
    AL_CHECK(assert_symbol(mod_sym));
    const auto module_name = mod_sym->to_string();

    bool import_all         = false;
    std::string module_file = module_name;
    std::string import_as   = module_name;

    if (contains(obj, ":all")) { import_all = true; }

    auto [file, file_succ] = get_next(obj, ":file");
    if (file_succ and pstring(file)) { module_file = file->to_string(); }

    auto [name_sexp, as_succ] = get_next(obj, ":as");
    if (as_succ)
    {
        auto new_name = eval->eval(name_sexp);
        if (psym(new_name)) { import_as = new_name->to_string(); }
    }

    if (env->module_loaded(module_name))
    {

        env->alias_module(module_name, import_as);

        if (import_all) { env->import_root_scope(module_name, env->current_module()); }

        return Qt;
    }

    // check if this is a built in module
    if (env->load_builtin_module(module_name, eval))
    {

        env->alias_module(module_name, import_as);

        if (import_all) { env->import_root_scope(module_name, env->current_module()); }

        return Qt;
    }

    for (auto &path : *Vmodpaths)
    {
        for (auto &postfix : { "", ".so", ".al" })
        {
            const auto eval_file = fs::path(path->to_string()) / fs::path(module_file + postfix);

            AL_DEBUG("Testing module file: "s += eval_file.string());

            if (!fs::exists(eval_file)) { continue; }

            if (fs::equivalent(eval_file, eval->get_current_file())) { continue; }

            // std::cout << eval_file << "\n";
            if (hash::hash(std::string_view(postfix)) == hash::hash(".so"))
            {
                env->load_module(eval, eval_file.string(), module_name);
                if (import_all) { env->import_root_scope(module_name, env->current_module()); }
                return Qt;
            }

            env->define_module(module_name, import_as);
            env->alias_module(module_name, import_as);
            env::detail::ModuleChange mc{ *env, module_name };
            eval->eval_file(eval_file);
            if (import_all) { env->import_root_scope(module_name, mc.old_module()); }

            return Qt;
        }
    }

    throw module_error(mod_sym->to_string(), "The module's file \"" + module_file + "\" was not found.");

    return Qnil;
}

ALObjectPtr Fdefvar(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));
    AL_CHECK(assert_max_size<3>(obj));
    AL_CHECK(assert_symbol(obj->i(0)));

    if (obj->size() >= 3 and pstring(obj->i(2)))
    {
        env->define_variable(obj->i(0), eval->eval(obj->i(1)), obj->i(2)->to_string());
        return Qt;
    };

    env->define_variable(obj->i(0), eval->eval(obj->i(1)), "");

    return Qt;
}

ALObjectPtr Fdefun(ALObjectPtr obj, env::Environment *env, eval::Evaluator *)
{
    AL_CHECK(assert_min_size<2>(obj));
    AL_CHECK(assert_symbol(obj->i(0)));
    AL_CHECK(assert_list(obj->i(1)));

    AL_CHECK(if (!detail::check_arg_list(obj->i(1))) {
        signal(Qdefun_signal, "Invalud argument list:", dump(obj->i(1)));
        return Qnil;
    });

    if (obj->size() >= 3 and pstring(obj->i(2)))
    {
        env->define_function(obj->i(0), obj->i(1), splice(obj, 3), obj->i(2)->to_string());
        return Qt;
    };

    env->define_function(obj->i(0), obj->i(1), splice(obj, 2));
    return Qt;
}

ALObjectPtr Fmodref(ALObjectPtr obj, env::Environment *env, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(obj));
    AL_DEBUG("Referecing symbol in module: "s += dump(obj));
    size_t curr_index = 0;
    auto curr_mod     = env->get_module(env->current_module());
    while (curr_index < obj->length() - 1)
    {
        auto next_sym = eval->eval(obj->i(curr_index));
        AL_CHECK(assert_symbol(next_sym));
        auto next_mod = curr_mod->get_module(next_sym->to_string());
        if (!next_mod) { throw module_refence_error(curr_mod->name(), next_sym->to_string()); }
        curr_mod = next_mod;
        ++curr_index;
    }

    auto next_sym = eval->eval(obj->i(curr_index));
    AL_CHECK(assert_symbol(next_sym));
    auto sym = curr_mod->get_symbol(next_sym->to_string());
    if (!sym) { throw module_refence_error(curr_mod->name(), next_sym->to_string(), true); }
    return sym;
}

ALObjectPtr Fdefmacro(ALObjectPtr obj, env::Environment *env, eval::Evaluator *)
{
    AL_CHECK(assert_min_size<2>(obj));
    AL_CHECK(assert_symbol(obj->i(0)));
    AL_CHECK(assert_list(obj->i(1)));

    AL_CHECK(if (!detail::check_arg_list(obj->i(1))) {
        signal(Qdefun_signal, "Invalud argument list:", dump(obj->i(1)));
        return Qnil;
    });

    if (obj->size() >= 3 and pstring(obj->i(2)))
    {
        env->define_macro(obj->i(0), obj->i(1), splice(obj, 3), obj->i(2)->to_string());
        return Qt;
    };

    env->define_macro(obj->i(0), obj->i(1), splice(obj, 2));

    return Qt;
}

ALObjectPtr Flambda(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_min_size<1>(obj));
    AL_CHECK(assert_list(obj->i(0)));

    AL_CHECK(if (!detail::check_arg_list(obj->i(0))) {
        signal(Qdefun_signal, "Invalud argument list:", dump(obj->i(1)));
        return Qnil;
    });

    auto new_lambda = make_object(obj->i(0), splice(obj, 1));
    new_lambda->set_function_flag();
    new_lambda->set_prop("--name--", make_string("lambda"));

    return new_lambda;
}

ALObjectPtr Fsetq(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<2>(obj));

    const auto len = std::size(*obj);
    for (size_t i = 0; i < len; i += 2)
    {
        AL_CHECK(assert_symbol(obj->i(i)));
        if (i + 1 >= len) { return Qnil; }
        auto new_val = evl->eval(obj->i(i + 1));
        env->update(obj->i(i), new_val);
    }

    return Qt;
}

ALObjectPtr Feval(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    return evl->eval(evl->eval(obj->i(0)));
}

ALObjectPtr Fset(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));

    auto sym = evl->eval(obj->i(0));

    AL_CHECK(assert_symbol(sym));

    auto new_val = evl->eval(obj->i(1));
    env->update(sym, new_val);
    return Qt;
}

ALObjectPtr Fquote(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<1>(obj));
    return obj->i(0);
}

ALObjectPtr Ffunction(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<1>(obj));
    return obj->i(0);
}

ALObjectPtr Fbackquote(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_size<1>(obj));
    if (!plist(obj->i(0))) { return obj->i(0); }
    return detail::handle_backquote_list(obj->i(0), eval);
}

ALObjectPtr Fif(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<2>(obj));

    if (is_truthy(evl->eval(obj->i(0)))) { return evl->eval(obj->i(1)); }
    else if (obj->length() == 3)
    {
        return eval_list(evl, obj, 2);
    }
    else
    {
        return Qnil;
    }
}

ALObjectPtr Fwhile(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<1>(obj));

    try
    {
        while (is_truthy(evl->eval(obj->i(0))))
        {
            try
            {
                eval_list(evl, obj, 1);
            }
            catch (al_continue &)
            {
                continue;
            }
        }
    }
    catch (al_break &)
    {
    }

    return Qt;
}

ALObjectPtr Fwhen(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<2>(obj));

    if (is_truthy(evl->eval(obj->i(0)))) { return eval_list(evl, obj, 1); }
    return Qnil;
}

ALObjectPtr Funless(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<2>(obj));

    if (!is_truthy(evl->eval(obj->i(0)))) { return eval_list(evl, obj, 1); }
    return Qnil;
}

ALObjectPtr Fdolist(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{

    AL_CHECK(assert_min_size<1>(obj));

    auto var_and_list = obj->i(0);
    auto bound_sym    = var_and_list->i(0);
    auto list         = evl->eval(var_and_list->i(1));

    env::detail::ScopePushPop spp{ *env };

    env->put(bound_sym, Qnil);

    try
    {
        for (auto list_element : list->children())
        {
            try
            {
                env->update(bound_sym, list_element);
                eval_list(evl, obj, 1);
            }
            catch (al_continue &)
            {
                continue;
            }
        }
    }
    catch (al_break &)
    {
    }

    return Qt;
}

ALObjectPtr Fdotimes(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{

    AL_CHECK(assert_min_size<1>(obj));

    auto var_and_times = obj->i(0);
    AL_CHECK(assert_list(var_and_times));
    AL_CHECK(assert_size<2>(var_and_times));

    auto bound_sym = var_and_times->i(0);
    AL_CHECK(assert_symbol(bound_sym));

    auto times = evl->eval(var_and_times->i(1));
    AL_CHECK(assert_int(times));

    env::detail::ScopePushPop spp{ *env };

    env->put(bound_sym, Qnil);

    try
    {
        for (int i = 0; i < times->to_int(); ++i)
        {
            try
            {
                env->update(bound_sym, make_int(i));
                eval_list(evl, obj, 1);
            }
            catch (al_continue &)
            {
                continue;
            }
        }
    }
    catch (al_break &)
    {
    }

    return Qt;
}

ALObjectPtr Fcond(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_list(obj));

    for (auto condition : *obj)
    {
        if (is_truthy(evl->eval(condition->i(0)))) { return eval_list(evl, condition, 1); }
    }
    return Qnil;
}

ALObjectPtr Fsignal(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));

    auto sym  = evl->eval(obj->i(0));
    auto data = evl->eval(obj->i(1));

    AL_CHECK(assert_symbol(sym));
    AL_CHECK(assert_list(data));

    throw signal_exception(sym, data);

    return Qt;
}

ALObjectPtr Ffuncall(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<1>(obj));

    auto fun_obj = eval->eval(obj->i(0));
    auto args    = eval_transform(eval, splice(obj, 1));


    return eval->handle_lambda(fun_obj, args);
}

ALObjectPtr Fprogn(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    return eval_list(evl, obj, 0);
}

ALObjectPtr Fprogn1(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    return eval_list_1(evl, obj, 0);
}

ALObjectPtr Fprogn2(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    return eval_list_2(evl, obj, 0);
}

ALObjectPtr Flet(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<1>(obj));
    AL_CHECK(assert_list(obj->i(0)));

    auto varlist = obj->i(0);

    std::vector<std::pair<ALObjectPtr, ALObjectPtr>> cells;
    cells.reserve(std::size(varlist->children()));

    for (auto var : varlist->children())
    {
        if (plist(var))
        {
            AL_CHECK(assert_size<2>(var));
            cells.push_back({ var->i(0), evl->eval(var->i(1)) });
        }
        else
        {
            AL_CHECK(assert_symbol(var));
            cells.push_back({ var, Qnil });
        }
    }

    env::detail::ScopePushPop spp{ *env };

    for (auto [ob, cell] : cells) { env->put(ob, cell); }

    return eval_list(evl, obj, 1);
}

ALObjectPtr Fletx(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<1>(obj));
    AL_CHECK(assert_list(obj->i(0)));

    env::detail::ScopePushPop spp{ *env };

    auto varlist = obj->i(0);
    for (auto var : varlist->children())
    {

        if (plist(var))
        {
            AL_CHECK(assert_size<2>(var));
            env->put(var->i(0), evl->eval(var->i(1)));
        }
        else
        {
            AL_CHECK(assert_symbol(var));
            env->put(var, Qnil);
        }
    }

    return eval_list(evl, obj, 1);
}

ALObjectPtr Fexit(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_max_size<1>(obj));
    if (obj->size() == 0) { throw al_exit(0); }
    auto val = evl->eval(obj->i(0));
    AL_CHECK(assert_int(val));
    throw al_exit(static_cast<int>(val->to_int()));
    return Qnil;
}

ALObjectPtr Fassert(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    auto val = evl->eval(obj->i(0));

    if (is_falsy(val))
    {
        throw signal_exception(
          env::intern("assert-signal"),
          make_object(make_string("Assertion failed."), make_string(dump(obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Fassert_not(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<1>(obj));
    auto val = evl->eval(obj->i(0));

    if (is_truthy(val))
    {
        throw signal_exception(
          env::intern("assert-signal"),
          make_object(make_string("Assertion failed."), make_string(dump(obj->i(0))), make_string(dump(val))));
    }
    return Qt;
}

ALObjectPtr Feq(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto ob_1 = evl->eval(obj->i(0));
    auto ob_2 = evl->eval(obj->i(1));

    return eq(ob_1, ob_2) ? Qt : Qnil;
}

ALObjectPtr Fequal(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_size<2>(obj));
    auto ob_1 = evl->eval(obj->i(0));
    auto ob_2 = evl->eval(obj->i(1));

    return equal(ob_1, ob_2) ? Qt : Qnil;
}

ALObjectPtr Freturn(ALObjectPtr obj, env::Environment *, eval::Evaluator *evl)
{
    AL_CHECK(assert_min_size<1>(obj));
    if (std::size(*obj) == 0) { throw al_return(Qnil); }
    auto val = evl->eval(obj->i(0));
    throw al_return(val);
    return Qnil;
}

ALObjectPtr Fbreak(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(obj));
    throw al_break();
    return Qnil;
}

ALObjectPtr Fcontinue(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    AL_CHECK(assert_size<0>(obj));
    throw al_continue();
    return Qnil;
}

ALObjectPtr Fsym_list(ALObjectPtr obj, env::Environment *env, eval::Evaluator *evl)
{
    AL_CHECK(assert_max_size<1>(obj));

    if (std::size(*obj) == 1)
    {
        auto package = evl->eval(obj->i(0));
        AL_CHECK(assert_symbol(package));
        ALObject::list_type syms;
        auto mod = env->get_module(package->to_string());
        if (mod == nullptr) { return Qnil; }
        for (auto &[sym, _] : mod->get_root()) { syms.push_back(env::intern(sym)); }
        return make_list(syms);
    }

    ALObject::list_type syms;
    auto mod = env->current_module_ref();
    for (auto &[sym, _] : mod.get_root()) { syms.push_back(env::intern(sym)); }
    for (auto &[sym, _] : env::Environment::g_global_symbol_table) { syms.push_back(env::intern(sym)); }

    return make_list(syms);
}

ALObjectPtr Fcondition_case(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    AL_CHECK(assert_min_size<2>(obj));
    AL_CHECK(assert_symbol(obj->i(0)));


    try
    {
        eval::detail::CatchTrack ct{ *eval };
        auto body = eval->eval(obj->i(1));
        return body;
    }
    catch (...)
    {
        return detail::catch_case_lippincott(obj, eval);
    }


    return Qnil;
}


}  // namespace alisp
