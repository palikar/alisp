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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_declarations.hpp"
#include "alisp/alisp/alisp_assertions.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{

namespace detail
{

static ALObjectPtr handle_backquote_list(ALObjectPtr obj, eval::Evaluator* eval)
{
    if (not plist(obj)) { return obj; }

    if (obj->i(0) == Qcomma) {
        return eval->eval(obj->i(1));
    }
    
    ALObject::list_type new_elements;


    for (auto el : *obj) {

        if (!plist(el)) {
            new_elements.push_back(el);
            continue;
        }

        if (el->i(0) == Qcomma_at) {
            auto new_list =  eval->eval(el->i(1));
            if (plist(new_list)) {
                new_elements.insert(new_elements.end(),
                                    std::begin(new_list->children()),
                                    std::end(new_list->children()));
            } else {
                new_elements.push_back(new_list);
            }
            continue;
        }
        
        new_elements.push_back(handle_backquote_list(el, eval));        
        
    }
    
    return make_object(new_elements);
}

}

ALObjectPtr Fimport(ALObjectPtr obj, env::Environment* env, eval::Evaluator* eval)
{
    namespace fs = std::filesystem;
    assert_min_size<1>(obj);

    auto mod_sym = eval->eval(obj->i(0));
    assert_symbol(mod_sym);
    const auto module_name = mod_sym->to_string();

    bool import_all = false;
    std::string module_file = module_name;
    std::string import_as = module_name;
    
    if (contains(obj, ":all")) { import_all = true; }

    auto [file, file_succ] = get_next(obj, ":file");
    if (file_succ and pstring(file)) {
        module_file = file->to_string();
    }

    auto [name_sexp, as_succ] = get_next(obj, ":as");
    if ( as_succ ) {
        auto new_name = eval->eval(name_sexp);
        if (psym(new_name)){
            import_as = new_name->to_string();
        }
    }

    if (env->module_loaded(module_name)) {
        
        env->alias_module(module_name, import_as);

        if (import_all ) {
            env->import_root_scope(module_name, env->current_module());
        }
        
        return Qt;
    }

    //check if this is a built in module
    if (env->load_builtin_module(module_name, eval)) {
        
        env->alias_module(module_name, import_as);
        
        if (import_all ) {
            env->import_root_scope(module_name, env->current_module());
        }
        
        return Qt;

    }
    
    env->define_module(module_name, import_as);
    env->alias_module(module_name, import_as);
    env::detail::ModuleChange mc{*env, module_name};

    for (auto& path : *Vmodpaths) {
        for (auto& postfix : std::vector<std::string>{"", ".al"}) {

            const auto eval_file = fs::path(path->to_string()) / fs::path(module_file + postfix);

            if(!fs::exists(eval_file)) { continue; }
        
            eval->eval_file(eval_file);

            if (import_all ) {
                env->import_root_scope(module_name, mc.old_module());
                return Qt;
            }
            
            return Qt;
        }        
    }

    throw module_error(mod_sym->to_string(), "The module's file \"" + module_file + "\" was not found.");

    return Qnil;
}

ALObjectPtr Fdefvar(ALObjectPtr obj, env::Environment* env, eval::Evaluator* eval)
{
    assert_size<2>(obj);
    assert_symbol(obj->i(0));

    env->define_variable(obj->i(0), eval->eval(obj->i(1)));

    return Qt;
}

ALObjectPtr Fdefun(ALObjectPtr obj, env::Environment* env, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    assert_symbol(obj->i(0));
    assert_list(obj->i(1));


    env->define_function(obj->i(0), obj->i(1), splice(obj, 2));
    return Qt;
}

ALObjectPtr Fmodref(ALObjectPtr obj, env::Environment* env, eval::Evaluator* eval)
{
    assert_min_size<1>(obj);

    size_t curr_index = 0;
    auto curr_mod = env->get_module(env->current_module());
    while (curr_index < obj->length() - 1) {
        auto next_sym = eval->eval(obj->i(curr_index));
        assert_symbol(next_sym);
        auto next_mod = curr_mod->get_module(next_sym->to_string());
        if (!next_mod) { throw module_refence_error(curr_mod->name(), next_sym->to_string()); }
        curr_mod = next_mod;
        ++curr_index;
    }
    
    auto next_sym = eval->eval(obj->i(curr_index));
    assert_symbol(next_sym);
    auto sym = curr_mod->get_symbol(next_sym->to_string());
    if(!sym) { throw module_refence_error(curr_mod->name(), next_sym->to_string(), true); }
    return sym;
}

ALObjectPtr Fdefmacro(ALObjectPtr obj, env::Environment* env, eval::Evaluator*)
{
    assert_min_size<2>(obj);
    assert_symbol(obj->i(0));
    assert_list(obj->i(1));

    env->define_macro(obj->i(0), obj->i(1), splice(obj, 2));

    return Qt;
}

ALObjectPtr Flambda(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_min_size<1>(obj);
    assert_list(obj->i(0));

    auto new_lambda = make_object(obj->i(0), splice(obj, 1));
    // auto new_lambda = obj;
    new_lambda->set_function_flag();
    
    return new_lambda;
}

ALObjectPtr Fsetq(ALObjectPtr obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_size<2>(obj);
    assert_symbol(obj->i(0));

    auto new_val = evl->eval(obj->i(1));
    env->update(obj->i(0), new_val);
    return Qt;

}

ALObjectPtr Fset(ALObjectPtr obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_size<2>(obj);

    auto sym = evl->eval(obj->i(0));

    assert_symbol(sym);

    auto new_val = evl->eval(obj->i(1));
    env->update(sym, new_val);
    return Qt;
}

ALObjectPtr Fquote(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return obj->i(0);
}

ALObjectPtr Ffunction(ALObjectPtr obj, env::Environment*, eval::Evaluator*)
{
    assert_size<1>(obj);
    return obj->i(0);
}

ALObjectPtr Fbackquote(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_size<1>(obj);
    if (! plist(obj->i(0))) { return obj->i(0); }
    return detail::handle_backquote_list(obj->i(0), eval);
}

ALObjectPtr Fif(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<2>(obj);

    if (is_truthy(evl->eval(obj->i(0)))) {
        return evl->eval(obj->i(1));
    } else if (obj->length() == 3) {
        return evl->eval(obj->i(2));
    } else {
        return Qnil;
    }
}

ALObjectPtr Fwhile(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<1>(obj);

    while (is_truthy(evl->eval(obj->i(0)))) {
        eval_list(evl, obj, 1);
    }
    return Qt;
}

ALObjectPtr Fwhen(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<2>(obj);

    if (is_truthy(evl->eval(obj->i(0)))) {
        return eval_list(evl, obj, 1);;
    }
    return Qnil;
}

ALObjectPtr Funless(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_min_size<2>(obj);

    if (!is_truthy(evl->eval(obj->i(0)))) {
        return eval_list(evl, obj, 1);;
    }
    return Qnil;
}

ALObjectPtr Fdolist(ALObjectPtr obj, env::Environment* env, eval::Evaluator* evl)
{

    assert_min_size<1>(obj);

    auto var_and_list = obj->i(0);
    auto bound_sym = var_and_list->i(0);
    auto list = evl->eval(var_and_list->i(1));

    env::detail::ScopePushPop spp{*env};

    env->put(bound_sym, Qnil);

    for (auto list_element : list->children())
    {
        env->update(bound_sym, list_element);

        eval_list(evl, obj, 1);
    }

    return Qt;

}

ALObjectPtr Fcond(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_list(obj);

    for (auto condition : *obj)
    {
        if (is_truthy(evl->eval(condition->i(0))))
        {
            return eval_list(evl, condition, 1);
        }
    }
    return Qnil;
}

ALObjectPtr Fsignal(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<2>(obj);

    auto sym = evl->eval(obj->i(0));
    auto data = evl->eval(obj->i(1));

    assert_symbol(sym);
    assert_list(sym);

    throw signal_exception(sym, data);

    return Qt;
}

ALObjectPtr Ffuncall(ALObjectPtr obj, env::Environment*, eval::Evaluator* eval)
{
    assert_min_size<1>(obj);

    auto fun_obj = eval->eval(obj->i(0));
    auto args = eval_transform(eval, splice(obj, 1));


    return eval->handle_lambda(fun_obj, args);

}

ALObjectPtr Fprogn(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    return eval_list(evl, obj, 1);
}

ALObjectPtr Flet(ALObjectPtr obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_min_size<1>(obj);
    assert_list(obj->i(0));

    auto varlist = obj->i(0);

    std::vector<std::pair<ALObjectPtr,ALObjectPtr>> cells;
    cells.reserve(std::size(varlist->children()));

    for (auto var : varlist->children()) {
        if (plist(var)) {
            assert_size<2>(var);
            cells.push_back({var->i(0), evl->eval(var->i(1))});
        } else {
            assert_symbol(var);
            cells.push_back({var, Qnil});
        }
    }

    env::detail::ScopePushPop spp{*env};

    for (auto[ob, cell] : cells) {
        env->put(ob, cell);
    }

    return eval_list(evl, obj, 1);
}

ALObjectPtr Fletx(ALObjectPtr obj, env::Environment* env, eval::Evaluator* evl)
{
    assert_min_size<1>(obj);
    assert_list(obj->i(0));

    env::detail::ScopePushPop spp{*env};

    auto varlist = obj->i(0);
    for (auto var : varlist->children()) {

        if (plist(var)) {
            assert_size<2>(var);
            env->put(var->i(0), evl->eval(var->i(1)));
        } else {
            assert_symbol(var);
            env->put(var, Qnil);
        }

    }

    return eval_list(evl, obj, 1);
}

ALObjectPtr Fexit(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    auto val = evl->eval(obj->i(0));
    assert_int(val);
    throw al_exit(static_cast<int>(val->to_int()));
    return Qnil;
}

ALObjectPtr Freturn(ALObjectPtr obj, env::Environment*, eval::Evaluator* evl)
{
    assert_size<1>(obj);
    auto val = evl->eval(obj->i(0));
    throw al_return(val);
    return Qnil;
}

}
