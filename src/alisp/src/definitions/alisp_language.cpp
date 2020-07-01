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
#include <unistd.h>

#include <algorithm>
#include <filesystem>
#include <string>

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_loadable_modules.hpp"

#include "alisp/alisp/declarations/language_constructs.hpp"

#include "alisp/utility/files.hpp"
#include "alisp/utility/macros.hpp"
#include "alisp/utility/hash.hpp"

#include "alisp/alisp/async/timing.hpp"

namespace alisp
{

namespace detail
{

static ALObjectPtr handle_backquote_list(ALObjectPtr obj, eval::Evaluator *eval)
{
    if (not plist(obj))
    {
        return obj;
    }

    if (obj->i(0) == Qcomma)
    {
        return eval->eval(obj->i(1));
    }

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
    if (t_list == Qnil)
    {
        return true;
    }

    for (auto &el : *t_list)
    {
        if (!psym(el))
        {
            return false;
        }
    }


    if (std::count(std::begin(*t_list), std::end(*t_list), Qrest) > 1)
    {
        return false;
    }


    if (std::count(std::begin(*t_list), std::end(*t_list), Qoptional) > 1)
    {
        return false;
    }

    auto rest_pos = std::find(std::begin(*t_list), std::end(*t_list), Qrest);
    auto opt_pos  = std::find(std::begin(*t_list), std::end(*t_list), Qoptional);
    if (rest_pos != std::end(*t_list) and opt_pos != std::end(*t_list) and rest_pos < opt_pos)
    {
        return false;
    }


    return true;
}

template<typename EXC>
static ALObjectPtr handle_exception(EXC p_exc, ALObjectPtr t_obj, env::Environment *env, eval::Evaluator *eval)
{

    for (size_t i = 2; i < t_obj->size(); ++i)
    {
        auto handler = t_obj->i(i);
        auto sym     = eval->eval(handler->i(0));
        AL_CHECK(assert_symbol(sym));
        AL_CHECK(assert_min_size<1>(handler));
        if (sym->to_string().compare(p_exc.name()) == 0)
        {
            if (t_obj->i(0) != Qnil)
            {
                env::detail::ScopePushPop cpp{ *env };
                env->put(t_obj->i(0),
                         make_object(make_int(static_cast<size_t>(p_exc.tag())), make_string(p_exc.what())));
            }
            return eval_list(eval, handler, 1);
        }
    }
    throw;
    return nullptr;
}

static ALObjectPtr catch_case_lippincott(ALObjectPtr t_obj, env::Environment *env, eval::Evaluator *eval)
{

    try
    {
        throw;
    }
    catch (environment_error &p_exc)
    {
        return handle_exception(p_exc, t_obj, env, eval);
    }
    catch (eval_error &p_exc)
    {
        return handle_exception(p_exc, t_obj, env, eval);
    }
    catch (signal_exception &p_exc)
    {
        for (size_t i = 2; i < t_obj->size(); ++i)
        {
            auto handler = t_obj->i(i);
            auto sym     = eval->eval(handler->i(0));
            AL_CHECK(assert_symbol(sym));
            AL_CHECK(assert_min_size<1>(handler));
            if (sym->to_string().compare(p_exc.name()) == 0)
            {
                if (t_obj->i(0) != Qnil)
                {
                    env::detail::ScopePushPop cpp{ *env };
                    env->put(t_obj->i(0), p_exc.m_list);
                }
                return eval_list(eval, handler, 1);
            }
        }
        throw;
    }
    catch (argument_error &p_exc)
    {
        return handle_exception(p_exc, t_obj, env, eval);
    }
    catch (module_error &p_exc)
    {
        return handle_exception(p_exc, t_obj, env, eval);
    }
    catch (module_refence_error &p_exc)
    {
        return handle_exception(p_exc, t_obj, env, eval);
    }
    catch (interrupt_error &p_exc)
    {
        return handle_exception(p_exc, t_obj, env, eval);
    }
    catch (...)
    {
        throw;
    }

    return Qt;
}

}  // namespace detail


struct Simport
{
    inline static const std::string name = "import";

    inline static const std::string doc{ R"((import MODULE [:file file] [:all] [( [(SYM MAPPED)]... )])

Import the module MODULE. MODULE should be a symbol and the imported
module should be in a file with the name of this symbol. The file
should be located somewhere on the ALISPPATH. An alternative file name
can be given through the :file keyword-argument. If the :all
keyword-argument is given. all of the symbols in MODULE will be
imported in the root scope of the current module. The last argument is
an optional list of mappings between symbols in the imported modules
and new symbols to be imported in the current module.

Example:
```elisp
(import 'fileio)
(import 'fileio :all)
(import 'fileio :file "../fileio.al")
(import 'fileio :all (f-exists exists))
```
)" };

    static ALObjectPtr Fimport(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
    {
        namespace fs = std::filesystem;
        AL_CHECK(assert_min_size<1>(obj));

        auto mod_sym = eval_check(eval, obj, 0, &assert_symbol<size_t>);

        const auto module_name = mod_sym->to_string();

        bool import_all         = false;
        std::string module_file = module_name;
        std::string import_as   = module_name;

        if (contains(obj, ":all"))
        {
            import_all = true;
        }

        auto [file, file_succ] = get_next(obj, ":file");
        if (file_succ and pstring(file))
        {
            module_file = file->to_string();
        }

        auto [name_sexp, as_succ] = get_next(obj, ":as");
        if (as_succ)
        {
            auto new_name = eval->eval(name_sexp);
            if (psym(new_name))
            {
                import_as = new_name->to_string();
            }
        }

        if (env->module_loaded(module_name))
        {

            env->alias_module(module_name, import_as);

            if (import_all)
            {
                env->import_root_scope(module_name, env->current_module());
            }

            return Qt;
        }

        // check if this is a built in module
        if (env->load_builtin_module(module_name, eval))
        {

            env->alias_module(module_name, import_as);

            if (import_all)
            {
                env->import_root_scope(module_name, env->current_module());
            }

            return Qt;
        }

        for (auto &path : *Vmodpaths)
        {
            for (auto &postfix : { "", ".so", ".al" })
            {
                const auto eval_file = fs::path(path->to_string()) / fs::path(module_file + postfix);

                AL_DEBUG("Testing module file: "s += eval_file.string());

                if (!fs::exists(eval_file))
                {
                    continue;
                }

                if (fs::equivalent(eval_file, eval->get_current_file()))
                {
                    continue;
                }

                // std::cout << eval_file << ": " << utility::check_elf(eval_file) << "\n";
                if (hash::hash(std::string_view(postfix)) == hash::hash(".so") or utility::check_elf(eval_file))
                {
                    env->load_module(eval, eval_file.string(), module_name);
                    if (import_all)
                    {
                        env->import_root_scope(module_name, env->current_module());
                    }
                    return Qt;
                }

                env->define_module(module_name, import_as);
                env->alias_module(module_name, import_as);
                env::detail::ModuleChange mc{ *env, module_name };
                eval->eval_file(eval_file);
                if (import_all)
                {
                    env->import_root_scope(module_name, mc.old_module());
                }

                return Qt;
            }
        }

        throw module_error(mod_sym->to_string(), "The module's file \"" + module_file + "\" was not found.");

        return Qnil;
    }
};

struct Sdefvar
{
    inline static const std::string name = "defvar";

    inline static const std::string doc{ R"((defvar NAME VALUE [DOC])

Define a new variable with a name `NAME` in the current
module. `VALUE` is the initial value of the variable and `DOC` is an
optional docstring. A variable *has* to be defines before used. A
variable defined through `defvar` will live till the end of the
program.

Example:
```elisp
(defvar new-var 42)
```

)" };

    static ALObjectPtr Fdefvar(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
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
};

struct Sdefconst
{
    inline static const std::string name = "defconst";

    inline static const std::string doc{ R"((defconst NAME VALUE [DOC])

Define a new constant variable with a name `NAME` in the current
module. `VALUE` is the initial value of the variable and `DOC` is an
optional docstring. A variable *has* to be defines before used. A
variable defined through `defconst` will live till the end of the
program. If another part of the porgram tries to chang a constant
variable, an error signal will be emitted.

Example:
```elisp
(defconst new-var 42)
```

)" };

    static ALObjectPtr Fdefconst(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<2>(obj));
        AL_CHECK(assert_max_size<3>(obj));
        AL_CHECK(assert_symbol(obj->i(0)));

        if (obj->size() >= 3 and pstring(obj->i(2)))
        {
            env->define_variable(obj->i(0), eval->eval(obj->i(1)), obj->i(2)->to_string(), true);
            return Qt;
        };

        env->define_variable(obj->i(0), eval->eval(obj->i(1)), "", true);

        return Qt;
    }
};

struct Sdefun
{
    inline static const std::string name = "import";

    inline static const std::string doc{ R"((import MODULE [:file file] [:all] [( [(SYM MAPPED)]... )])

Import the module MODULE. MODULE should be a symbol and the imported
module should be in a file with the name of this symbol. The file
should be located somewhere on the ALISPPATH. An alternative file name
can be given through the :file keyword-argument. If the :all
keyword-argument is given. all of the symbols in MODULE will be
imported in the root scope of the current module. The last argument is
an optional list of mappings between symbols in the imported modules
and new symbols to be imported in the current module.

Example:
```elisp
(import 'fileio)
(import 'fileio :all)
(import 'fileio :file "../fileio.al")
(import 'fileio :all (f-exists exists))
```
)" };

    static ALObjectPtr Fdefun(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *)
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
};

struct Smodref
{
    inline static const std::string name = "modref";

    inline static const std::string doc{ R"(((modref MODUE [[MODUE] ...] SYMBOL [[symbol] ...] )

Refrence a symbol in another module. The function can also be used to
reference symbols from submodules. That is, if a module imports
another module for itself, symbols in it can also be referenced. In
most circumstances you won't need this function as there is a
syntactic sugar for it - the dot syntax.

Example:
```elisp
(import 'fileio)

; those two are equivalent
((modref fileio f-exists) "../file.al")
(fileio.f-exists "../file.al")
```

The last argument of `modref` must be the symbol name. The previous
arguments should module names.
))" };

    static ALObjectPtr Fmodref(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
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
            if (!next_mod)
            {
                throw module_refence_error(curr_mod->name(), next_sym->to_string());
            }
            curr_mod = next_mod;
            ++curr_index;
        }

        auto next_sym = eval->eval(obj->i(curr_index));
        AL_CHECK(assert_symbol(next_sym));
        auto sym = curr_mod->get_symbol(next_sym->to_string());
        if (!sym)
        {
            throw module_refence_error(curr_mod->name(), next_sym->to_string(), true);
        }
        return sym;
    }
};

struct Sdefmacro
{
    inline static const std::string name = "defmacro";

    inline static const std::string doc{ R"(((defmacro NAME (ARGLIST) [DOC] BODY)

Define a new macro with a name `NAME` in the current
module. `ARGLIST` should be a valid argument list definition. `DOC` is
an optional docscring and `BODY` is a list of forms to be evaluated
when the function is called. As oppose to a function, the arguments of
a macro are not evaluated when the macro is called.

Example:
```elisp
(defmacro inc (x)
    `(setq x (+ 1 ,x)))
```
))" };

    static ALObjectPtr Fdefmacro(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *)
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
};

struct Slambda
{

    inline static const std::string name = "lambda";

    inline static const std::string doc{ R"((lambda (ARGLIST) BODY)


)" };

    static ALObjectPtr Flambda(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *)
    {
        AL_CHECK(assert_min_size<1>(obj));
        AL_CHECK(assert_list(obj->i(0)));

        AL_CHECK(if (!detail::check_arg_list(obj->i(0))) {
            signal(Qdefun_signal, "Invalid argument list:", dump(obj->i(1)));
            return Qnil;
        });

        auto new_lambda = make_object(obj->i(0), splice(obj, 1));
        new_lambda->set_function_flag();
        new_lambda->set_prop("--name--", make_string("lambda"));

        ALObject::list_type closure_list;
        for (const auto &scope : env->stack().current_frame())
        {
            for (const auto &[sym, val] : scope)
            {
                closure_list.push_back(make_object(make_string(sym), val));
            }
        }
        new_lambda->set_prop("--closure--", make_list(closure_list));

        return new_lambda;
    }
};

struct Ssetq
{
    inline static const std::string name = "setq";

    inline static const std::string doc{ R"((setq SYMBOL VALUE [[SYMBOL VALUE] ... ])

Set the value of the variable pointed by `SYMBOL` to
`VALUE`. `SYMBOL` will not be evaluated. `setq` can also be used to
set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(setq new-var 43)
```
)" };

    static ALObjectPtr Fsetq(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
    {
        AL_CHECK(assert_min_size<2>(obj));

        const auto len = std::size(*obj);
        for (size_t i = 0; i < len; i += 2)
        {
            AL_CHECK(assert_symbol(obj->i(i)));
            if (i + 1 >= len)
            {
                return Qnil;
            }
            auto new_val = evl->eval(obj->i(i + 1));
            env->update(obj->i(i), new_val);
        }

        return Qt;
    }
};

struct Seval
{
    inline static const std::string name = "eval";

    inline static const std::string doc{ R"((eval FORM)

Evaluate the form `FORM`. The usual form for evaluation apply.
)" };

    static ALObjectPtr Feval(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<1>(obj));
        return evl->eval(evl->eval(obj->i(0)));
    }
};

struct Sset
{
    inline static const std::string name = "set";

    inline static const std::string doc{ R"(((set FORM VALUE))

Set the value of the variable pointed by `FORM` to `VALUE`. `FORM`
will be evaluated and should return a symbol. `setq` can also be used
to set the value of multiple variables at once. All of the variables
should be defined beforehand.

Example:
```elisp
(defvar new-var 42)
(set 'new-var 43)
```
)" };

    static ALObjectPtr Fset(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<2>(obj));

        auto sym = evl->eval(obj->i(0));

        AL_CHECK(assert_symbol(sym));

        auto new_val = evl->eval(obj->i(1));
        env->update(sym, new_val);
        return Qt;
    }
};

struct Squote
{
    inline static const std::string name = "quote";

    inline static const std::string doc{ R"(((quote FORM)

Return `FORM`, without evaluating it. `(quote x)` yields ‘x’. `x is a
syntactic sugar for this function.
))" };

    static ALObjectPtr Fquote(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<1>(obj));
        return obj->i(0);
    }
};

struct Sfunction
{
    inline static const std::string name = "function";

    inline static const std::string doc{ R"((funtion OBJECT)

Return `OBJECT`, without evaluating it but setting its function flag
to true. `function` should be used to quote lambdas and other
callables.
)" };

    static ALObjectPtr Ffunction(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<1>(obj));
        return obj->i(0);
    }
};

struct Sbackquote
{
    inline static const std::string name = "backquote";

    inline static const std::string doc{ R"((backquote LIST)

Backquote the list `LIST`. `LIST is syntactic sugar for this function.

Example:
```elisp

`(val-1 ,val-2 ,@(val-3 val-3 )) ; '(val-1 (eval val-2) val-3 val-3)
```
)" };

    static ALObjectPtr Fbackquote(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        if (!plist(obj->i(0)))
        {
            return obj->i(0);
        }
        return detail::handle_backquote_list(obj->i(0), eval);
    }
};

struct Sif
{
    inline static const std::string name = "if";

    inline static const std::string doc{ R"((if CONDITION THEN ELSE)

Evaluate `CONDITION` and if its value is *truthy*, evaluate and return
the value of `THEN`. Otherwise evaluate and return the value of
`ELSE`.
)" };

    static ALObjectPtr Fif(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_min_size<2>(obj));

        if (is_truthy(evl->eval(obj->i(0))))
        {
            return evl->eval(obj->i(1));
        }
        else if (obj->length() >= 3)
        {
            return eval_list(evl, obj, 2);
        }
        else
        {
            return Qnil;
        }
    }
};

struct Swhile
{
    inline static const std::string name = "while";

    inline static const std::string doc{ R"((while CONDITION BODY)

Evaluate `BODY` as long as `CONDITION` evaluates to a value that is
*truthy*. `while` returns `nil`.
)" };

    static ALObjectPtr Fwhile(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
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
};

struct Swhen
{
    inline static const std::string name = "when";

    inline static const std::string doc{ R"((when CONDITION BODY)

Evaluate `BODY` if `CONDITION` evaluates to *truthy* value.
)" };

    static ALObjectPtr Fwhen(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_min_size<2>(obj));

        if (is_truthy(evl->eval(obj->i(0))))
        {
            return eval_list(evl, obj, 1);
        }
        return Qnil;
    }
};

struct Sunless

{
    inline static const std::string name = "unless";

    inline static const std::string doc{ R"((unless CONDITION BODY)

Evaluate `BODY` if `CONDITION` evaluates to *falsey* value.
)" };

    static ALObjectPtr Funless(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_min_size<2>(obj));

        if (!is_truthy(evl->eval(obj->i(0))))
        {
            return eval_list(evl, obj, 1);
        }
        return Qnil;
    }
};

struct Sdolist
{
    inline static const std::string name = "dolist";

    inline static const std::string doc{ R"((dolist (SYMBOL LIST) BODY)

Evaluate `BODY` for each symbol in `LIST` while bonding the respective
element to `SYMBOL`.

Example:
```elisp
(dolist (s '(1 2 3 4))
   (println s))
```

)" };

    static ALObjectPtr Fdolist(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
    {

        AL_CHECK(assert_min_size<1>(obj));

        auto var_and_list = obj->i(0);
        auto bound_sym    = var_and_list->i(0);
        auto list         = evl->eval(var_and_list->i(1));

        AL_CHECK(assert_symbol(bound_sym));
        if (equal(list, Qnil))
        {
            return Qnil;
        };

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
};

struct Sdotimes
{
    inline static const std::string name = "dotimes";

    inline static const std::string doc{ R"((dotimes (SYMBOL COUNT) BODY)

Evaluate `BODY` once for each integer from 0 (inclusive) to `COUNT` (exclusive), binding the variable `SYMBOL` to the integer for the current iteration.

Example:
```elisp
(dotimes (i 100)
   (println "i:" i ))
```

)" };

    static ALObjectPtr Fdotimes(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
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
};

struct Scond
{
    inline static const std::string name = "cond";

    inline static const std::string doc{ R"((cond [ ( [CODITION BODY] ) ... ])

Chooses what to evaluate among an arbitrary number of
alternatives. Each clause must a list. The first element of each list
will be evaluated and if its value is truthy, the rest of the elements
of the corresponging list will also be evaluated. The evaluation of
`cond` is then finished.

Example:
```elisp
(cond
((== (1  2)) (println "This won't print"))
((== (2  2)) (println "This will print")))
```
)" };

    static ALObjectPtr Fcond(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_list(obj));

        for (auto condition : *obj)
        {
            if (is_truthy(evl->eval(condition->i(0))))
            {
                return eval_list(evl, condition, 1);
            }
        }
        return Qnil;
    }
};

struct Ssignal
{
    inline static const std::string name = "signal";

    inline static const std::string doc{ R"((signal SYMBOL LIST)

Emit a signal with symbol `SYMBOL` and some arbitrary data stores in
`LIST`.

Example:
```elisp
(signal 'my-error '("there was an error" 42))
```
)" };

    static ALObjectPtr Fsignal(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<2>(obj));

        auto sym  = evl->eval(obj->i(0));
        auto data = evl->eval(obj->i(1));

        AL_CHECK(assert_symbol(sym));
        AL_CHECK(assert_list(data));

        throw signal_exception(sym, data);

        return Qt;
    }
};

struct Sfuncall
{
    inline static const std::string name = "funcall";

    inline static const std::string doc{ R"((funcall SYMBOL LIST)

Call the function pointed by `SYMBOL` and pass the symbols in `LIST`
as arguments.
)" };

    static ALObjectPtr Ffuncall(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<1>(obj));

        auto fun_obj = eval->eval(obj->i(0));
        auto args    = eval_transform(eval, splice(obj, 1));


        return eval->eval_callable(fun_obj, args);
    }
};

struct Sapply
{
    inline static const std::string name = "apply";

    inline static const std::string doc{ R"((apply SYMBOL (LIST))

Call the function pointed by `SYMBOL` and pass the symbols in `LIST`
as arguments.
)" };

    static ALObjectPtr Fapply(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<2>(obj));

        auto fun_obj = eval->eval(obj->i(0));
        auto args    = eval_transform(eval, eval->eval(obj->i(1)));
        return eval->eval_callable(fun_obj, args);
    }
};

struct Sprogn
{
    inline static const std::string name = "progn";

    inline static const std::string doc{ R"((progn BODY)

Evaluate the forms in `BODY` sequentially and return the value of the
last one.
)" };

    static ALObjectPtr Fprogn(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        return eval_list(evl, obj, 0);
    }
};

struct Sprogn1
{
    inline static const std::string name = "progn1";

    inline static const std::string doc{ R"((progn1 BODY)

Evaluate the forms in `BODY` sequentially and return the value of the
fist one.
)" };

    static ALObjectPtr Fprogn1(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        return eval_list_1(evl, obj, 0);
    }
};

struct Sprogn2
{
    inline static const std::string name = "progn2";

    inline static const std::string doc{ R"((progn2 BODY)

Evaluate the forms in `BODY` sequentially and return the value of the
second one.
)" };

    static ALObjectPtr Fprogn2(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        return eval_list_2(evl, obj, 0);
    }
};

struct Slet
{
    inline static const std::string name = "let";

    inline static const std::string doc{ R"((let ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)

Bind local variables and execute `BODY`. The second argument is a list
of forms like `(VARIABLE VALUE)`. Each `VALUE` will be evaluated and
its value will be bound to `VARIABLE`. `nil` variables can also be
declared without initial value.

Example:
```elisp
(let ((var-1 42)
      (var-2 "43")
       var-3)         ; nil variable
   (println var-1)    ; 42
   (println var-2))   ; 43
```
)" };

    static ALObjectPtr Flet(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
    {
        AL_CHECK(assert_min_size<1>(obj));
        AL_CHECK(assert_list(obj->i(0)));

        auto varlist = obj->i(0);

        std::vector<std::pair<ALObjectPtr, ALObjectPtr>> cells;
        cells.reserve(std::size(varlist->children()));

        env::detail::ScopePushPop spp{ *env };

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


        for (auto [ob, cell] : cells)
        {
            env->put(ob, cell);
        }

        return eval_list(evl, obj, 1);
    }
};

struct Sletx
{
    inline static const std::string name = "let*";

    inline static const std::string doc{ R"((let* ([[VAR]...] [[(VAR VALUE)] ...] ) BODY)

Bind local variables and execute `BODY`. In contrast `let`, each
variable can be used in the definition of the following variables.

Example:
```elisp
(let* ((var-1 42)
      (var-2 var-1)
       var-3)         ; nil variable
   (println var-1)    ; 42
   (println var-2))   ; 43
```
)" };

    static ALObjectPtr Fletx(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
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
};

struct Sexit
{
    inline static const std::string name = "exit";

    inline static const std::string doc{ R"((exit [FORM])

Exit the program. If `FORM` is given, its value will be the return
code of the process. Otherwise the return code will be 0.
)" };

    static ALObjectPtr Fexit(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_max_size<1>(obj));
        if (obj->size() == 0)
        {
            throw al_exit(0);
        }
        auto val = evl->eval(obj->i(0));
        AL_CHECK(assert_int(val));
        throw al_exit(static_cast<int>(val->to_int()));
        return Qnil;
    }
};

struct Sassert
{
    inline static const std::string name = "assert";

    inline static const std::string doc{ R"((assert FORM)

Assert that the value of `FORM` is *truthy*. If not, an assert signal
is emitted.

Example:
```elisp
(assert t)
(assert nil)
```
)" };

    static ALObjectPtr Fassert(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }

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
};

struct Sassert_not
{
    inline static const std::string name = "assert-not";

    inline static const std::string doc{ R"((assert-not FORM)

Assert that the value of `FORM` is *falsey*. If not, an assert signal
is emitted.

Example:
```elisp
(assert nil)
(assert t)
```
)" };

    static ALObjectPtr Fassert_not(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        if (is_falsy(Vdebug_mode))
        {
            return Qt;
        }
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
};

struct Seq
{
    inline static const std::string name = "eq";

    inline static const std::string doc{ R"((equal FORM1 FORM2)

Check if the values of `FORM1` and `FORM2` point to the same
object. If the values are ints or doubles, the actual values will be
tested for equality and `t` is return if they are equal. In all other
cases, return `t` only if the two objects are the same i.e a change in
one of the objects, will also change the other one.
)" };

    static ALObjectPtr Feq(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<2>(obj));
        auto ob_1 = evl->eval(obj->i(0));
        auto ob_2 = evl->eval(obj->i(1));

        return eq(ob_1, ob_2) ? Qt : Qnil;
    }
};

struct Sequal
{
    inline static const std::string name = "equal";

    inline static const std::string doc{ R"((equal FORM1 FORM2)

Return `t` if `FORM1`a and `FORM2` have the same value. Return `nil`
otherwise.
)" };

    static ALObjectPtr Fequal(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_size<2>(obj));
        auto ob_1 = evl->eval(obj->i(0));
        auto ob_2 = evl->eval(obj->i(1));

        return equal(ob_1, ob_2) ? Qt : Qnil;
    }
};

struct Sreturn
{
    inline static const std::string name = "return";

    inline static const std::string doc{ R"((return [FROM])

Return an optional value from a function. If `FROM` is given, it will
be evaluated and its value will be the return value of the
function. Otherwise `nil` is the returned value.

)" };

    static ALObjectPtr Freturn(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *evl)
    {
        AL_CHECK(assert_min_size<1>(obj));
        if (std::size(*obj) == 0)
        {
            throw al_return(Qnil);
        }
        auto val = evl->eval(obj->i(0));
        throw al_return(val);
        return Qnil;
    }
};

struct Sbreak
{
    inline static const std::string name = "break";

    inline static const std::string doc{ R"((break)

Break out of a loop.
)" };

    static ALObjectPtr Fbreak(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<0>(obj));
        throw al_break();
        return Qnil;
    }
};

struct Scontinue
{
    inline static const std::string name = "continue";

    inline static const std::string doc{ R"((continue)

Start a new loop iteration.
)" };

    static ALObjectPtr Fcontinue(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
    {
        AL_CHECK(assert_size<0>(obj));
        throw al_continue();
        return Qnil;
    }
};

struct Ssym_list
{
    inline static const std::string name = "symbols-list";

    inline static const std::string doc{ R"((symbols-list [PACKAGE])

Return a list the symbols that are defines in PACKAGE.
)" };

    static ALObjectPtr Fsym_list(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *evl)
    {
        AL_CHECK(assert_max_size<1>(obj));

        if (std::size(*obj) == 1)
        {
            auto package = evl->eval(obj->i(0));
            AL_CHECK(assert_symbol(package));
            ALObject::list_type syms;
            auto mod = env->get_module(package->to_string());
            if (mod == nullptr)
            {
                return Qnil;
            }
            for (auto &[sym, _] : mod->get_root())
            {
                syms.push_back(env::intern(sym));
            }
            return make_list(syms);
        }

        ALObject::list_type syms;
        auto mod = env->current_module_ref();
        for (auto &[sym, _] : mod.get_root())
        {
            syms.push_back(env::intern(sym));
        }
        for (auto &[sym, _] : env::Environment::g_internal_symbols)
        {
            syms.push_back(env::intern(sym));
        }

        return make_list(syms);
    }
};

struct Scondition_case
{
    inline static const std::string name = "condition-case";

    inline static const std::string doc{ R"((condition-case SYMBOL BODY [[HANDLERS]...])
)" };

    static ALObjectPtr Fcondition_case(const ALObjectPtr &obj, env::Environment *env, eval::Evaluator *eval)
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
            return detail::catch_case_lippincott(obj, env, eval);
        }


        return Qnil;
    }
};

struct Smake_symbol
{
    inline static const std::string name = "make-symbol";

    inline static const std::string doc{ R"((make-symbol NAME)

Return a new symbol with the name NAME.

Example:
```elisp
(make-symbol "sym")
```
)" };

    static ALObjectPtr Fmake_symbol(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto name = AL_EVAL(obj, eval, 0);
        AL_CHECK(assert_string(name));

        return make_symbol(name->to_string());
    }
};

struct Sintern
{
    inline static const std::string name = "intern";

    inline static const std::string doc{ R"(intern NAME)" };

    static ALObjectPtr Fintern(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto name = AL_EVAL(obj, eval, 0);
        AL_CHECK(assert_string(name));

        return env::intern(name->to_string());
    }
};

struct Snull
{
    inline static const std::string name = "null";

    inline static const std::string doc{ R"((null SYM)

Return `t` if `SYM` is `nil` and `t` otherwise.
)" };

    static ALObjectPtr Fnull(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(obj));
        auto sym = AL_EVAL(obj, eval, 0);

        if (plist(sym))
        {
            return AL_BOOL(sym->children().empty());
        }

        return AL_BOOL(equal(sym, Qnil));
    }
};


}  // namespace alisp
