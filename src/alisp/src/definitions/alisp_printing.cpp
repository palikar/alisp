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

#include "alisp/alisp/alisp_common.hpp"
#include "alisp/alisp/alisp_env.hpp"
#include "alisp/alisp/alisp_eval.hpp"
#include "alisp/alisp/alisp_object.hpp"
#include "alisp/alisp/alisp_exception.hpp"
#include "alisp/alisp/alisp_assertions.hpp"
#include "alisp/alisp/alisp_pattern_matching.hpp"
#include "alisp/alisp/alisp_streams.hpp"

#include "alisp/alisp/declarations/printing.hpp"

#include "alisp/utility/macros.hpp"


namespace alisp
{


struct Sprint
{
    inline static const std::string name = "print";

    inline static const std::string doc{ R"((print FORM [[FORM] ...])

Print the value of VALUE of form on the standard output stream.
)" };

    static ALObjectPtr Fprint(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<1>(t_obj));

        for (auto child : *t_obj)
        {
            auto val = eval->eval(child);

            make_visit(
              val,
              type(ALObjectType::INT_VALUE) >>= [](ALObjectPtr obj) { al::cout << obj->to_int(); },
              type(ALObjectType::REAL_VALUE) >>= [](ALObjectPtr obj) { al::cout << obj->to_real(); },
              type(ALObjectType::STRING_VALUE) >>= [](ALObjectPtr obj) { al::cout << obj->to_string(); },
              type(ALObjectType::SYMBOL) >>= [](ALObjectPtr obj) { al::cout << obj->to_string(); });
        }

        return Qt;
    }
};

struct Seprint
{
    inline static const std::string name = "eprint";

    inline static const std::string doc{ R"((eprint VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard error stream.
)" };

    static ALObjectPtr Feprint(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_min_size<1>(t_obj));

        for (auto child : *t_obj)
        {
            auto val = eval->eval(child);

            make_visit(
              val,
              type(ALObjectType::INT_VALUE) >>= [](ALObjectPtr obj) { al::cerr << obj->to_int(); },
              type(ALObjectType::REAL_VALUE) >>= [](ALObjectPtr obj) { al::cerr << obj->to_real(); },
              type(ALObjectType::STRING_VALUE) >>= [](ALObjectPtr obj) { al::cerr << obj->to_string(); },
              type(ALObjectType::SYMBOL) >>= [](ALObjectPtr obj) { al::cerr << obj->to_string(); });
        }

        return Qt;
    }
};

struct Sprintln
{
    inline static const std::string name = "println";

    inline static const std::string doc{ R"((println VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard output stream and put a new
line character.
)" };

    static ALObjectPtr Fprintln(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
    {
        Sprint::Fprint(t_obj, env, eval);
        al::cout << '\n';
        return Qt;
    }
};

struct Seprintln
{
    inline static const std::string name = "eprintln";

    inline static const std::string doc{ R"((eprintln VALUE [[VALUE] ...])

Print the value of VALUE of form on the standard error stream and put a new
line character.
)" };

    static ALObjectPtr Feprintln(const ALObjectPtr &t_obj, env::Environment *env, eval::Evaluator *eval)
    {
        Seprint::Feprint(t_obj, env, eval);
        al::cerr << '\n';
        return Qt;
    }
};

struct Sdump
{
    inline static const std::string name = "dump";

    inline static const std::string doc{ R"((dump FORM))" };

    static ALObjectPtr Fdump(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        std::cout << dump(eval->eval(t_obj->i(0))) << "\n";
        return Qt;
    }
};

struct Sdumpstack
{
    inline static const std::string name = "dumpstack";

    inline static const std::string doc{ R"((dumpstack)

Print a formatted version of the current state of the execution
environment. This is where the stack frames and the scopes live.
)" };

    static ALObjectPtr Fdumpstack(const ALObjectPtr &, env::Environment *env, eval::Evaluator *)
    {
        env->stack_dump();
        return Qt;
    }
};

struct Sdumpcallstack
{
    inline static const std::string name = "dumpcallstack";

    inline static const std::string doc{ R"((dumpcallstack)

Print a formatted version of the current call stack on the standard
output. This function is meant for debugging.
)" };

    static ALObjectPtr Fdumpcallstack(const ALObjectPtr &, env::Environment *env, eval::Evaluator *)
    {
        env->callstack_dump();
        return Qt;
    }
};

struct Sdumpsystem
{
    inline static const std::string name = "dumpsystem";

    inline static const std::string doc{ R"((dumpsystem)

Print out the currenttly bounded symbols in the interpreter. This
function is meant for debugging.
)" };

    static ALObjectPtr Fdumpsystem(const ALObjectPtr &, env::Environment *env, eval::Evaluator *)
    {
        env->env_dump();
        return Qt;
    }
};

struct Sdumplicense
{
    inline static const std::string name = "dumplicense";

    inline static const std::string doc{ R"((dumplicense)

Print the license of Alisp on the standard output.
)" };

    static ALObjectPtr Fdumplicense(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        al::cout << AL_LICENSE_TEXT << '\n';
        return Qt;
    }
};

struct Sdumpcredits
{
    inline static const std::string name = "dumpcredits";

    inline static const std::string doc{ R"((dumpcredits)

Print the contributors information for Alisp on the standard output.
)" };

    static ALObjectPtr Fdumpcredits(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        al::cout << AL_CREDITS_TEXT << '\n';
        return Qt;
    }
};

struct Sdumpbuildscript
{
    inline static const std::string name = "dumpbuildscript";

    inline static const std::string doc{ R"((dumpbuildscript)

Print the string that gets printed out on start of the repl interpreter.
)" };

    static ALObjectPtr Fdumpbuildscript(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        al::cout << get_build_info() << '\n';
        return Qt;
    }
};

struct Sread_line
{
    inline static const std::string name = "read-line";

    inline static const std::string doc{ R"((read-line)

Read a single line form the standard input stream and return it.
)" };

    static ALObjectPtr Fread_line(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        std::string line;
        return make_string(al::cin.get().get_line());
    }
};

struct Sread_char
{
    inline static const std::string name = "read-char";

    inline static const std::string doc{ R"((read-char)

Read a single character form the standard input stream and return it.
)" };

    static ALObjectPtr Fread_char(const ALObjectPtr &, env::Environment *, eval::Evaluator *)
    {
        return make_char(al::cin.get().get_char());
    }
};

struct Sread_chars
{
    inline static const std::string name = "read-chars";

    inline static const std::string doc{ R"((read-chars COUNT)

Read several characters form the standard input stream and return them.
)" };

    static ALObjectPtr Freaed_chars(const ALObjectPtr &t_obj, env::Environment *, eval::Evaluator *eval)
    {
        AL_CHECK(assert_size<1>(t_obj));
        auto count = eval_check(eval, t_obj, 0, &assert_int<size_t>);
        return make_string(al::cin.get().get_chars(static_cast<size_t>(count->to_int())));
    }
};

}  // namespace alisp
