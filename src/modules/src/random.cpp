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
#include <random>
#include <ctime>


namespace al_random
{
using namespace alisp;


namespace detail
{

static std::random_device rand_dev;
static std::mt19937 rand_eng(rand_dev());


}  // namespace detail


struct rand_int
{
    inline static const std::string name{ "rand-int" };

    inline static const Signature signature{ Int{}, Int{} };

    inline static const std::string doc{ R"((rand-int LOWER UPPER)

Return a random integer in the range [LOWER, UPPER]
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_int(a);
        auto b = arg_eval(eval, obj, 1);
        assert_int(b);

        std::uniform_int_distribution<> distr(static_cast<int>(a->to_int()), static_cast<int>(b->to_int()) + 1);

        return make_int(distr(detail::rand_eng));
    }
};

struct choice
{
    inline static const std::string name{ "choice" };

    inline static const Signature signature{ List{} };

    inline static const std::string doc{ R"((choice LIST)

Return a random element from the list `LIST`.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_list(a);

        std::uniform_int_distribution<> distr(0, static_cast<int>(a->size()) - 1);

        return a->children()[static_cast<size_t>(distr(detail::rand_eng))];
    }
};

struct sample
{
    inline static const std::string name{ "sample" };

    inline static const Signature signature{ List{}, Int{} };

    inline static const std::string doc{ R"((choice LIST CNT)

Return a list with `CNT` random elements from the list `LIST`.

)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_list(a);
        auto k = arg_eval(eval, obj, 1);
        assert_int(k);
        std::uniform_int_distribution<> distr(0, static_cast<int>(a->size()) - 1);
        ALObject::list_type lis;
        for (int i = 0; i < k->to_int(); ++i)
        {
            lis.push_back(a->children()[static_cast<size_t>(distr(detail::rand_eng))]);
        }

        return make_list(lis);
    }
};

struct uniform
{
    inline static const std::string name{ "uniform" };

    inline static const Signature signature{ Double{}, Double{} };

    inline static const std::string doc{ R"((uniform A B)

Return a random real number from an uniform distribution of [A, B].
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);
        auto b = arg_eval(eval, obj, 1);
        assert_number(b);

        std::uniform_real_distribution<> distr(a->to_real(), b->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct exponential
{
    inline static const std::string name{ "exponential" };

    inline static const Signature signature{ Double{} };

    inline static const std::string doc{ R"((exponential A)

Return a random real number from an exponential distribution with e=A
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);

        std::exponential_distribution<> distr(a->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct gamma
{
    inline static const std::string name{ "gamma" };

    inline static const Signature signature{ Double{}, Double{} };

    inline static const std::string doc{ R"((gamma A B)

Return a random real number from a gamma distribution with k=A, theta=B
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);
        auto b = arg_eval(eval, obj, 1);
        assert_number(b);

        std::gamma_distribution<> distr(a->to_real(), b->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct gauss
{
    inline static const std::string name{ "gauss" };

    inline static const Signature signature{ Double{}, Double{} };

    inline static const std::string doc{ R"((gauss A B)

Return a random real number from a gauss distribution with mean=A, std=B
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);
        auto b = arg_eval(eval, obj, 1);
        assert_number(b);

        std::normal_distribution<> distr(a->to_real(), b->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct lognorm
{
    inline static const std::string name{ "lognorm" };

    inline static const Signature signature{ Double{} };

    inline static const std::string doc{ R"((lognorm A)

Return a random real number from a log-normal distribution with mean=A, std=B
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);
        auto b = arg_eval(eval, obj, 1);
        assert_number(b);

        std::lognormal_distribution<> distr(a->to_real(), b->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct weibull
{
    inline static const std::string name{ "weibull" };

    inline static const Signature signature{ Double{}, Double{} };

    inline static const std::string doc{ R"((weibull A B)

Return a random real number from a weibull distribution with lambda=A, k=B
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);
        auto b = arg_eval(eval, obj, 1);
        assert_number(b);

        std::weibull_distribution<> distr(a->to_real(), b->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct geometric
{
    inline static const std::string name{ "geometric" };

    inline static const Signature signature{ Double{} };

    inline static const std::string doc{ R"((geometric A)

Return a random real number from a geometric distribution with p=A
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);

        std::geometric_distribution<> distr(a->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct fisher
{
    inline static const std::string name{ "fisher-f" };

    inline static const Signature signature{ Double{}, Double{} };

    inline static const std::string doc{ R"((fisher-f A B)

Return a random real number from a f-distribution distribution with m=A, n=B
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);
        auto b = arg_eval(eval, obj, 1);
        assert_number(b);

        std::fisher_f_distribution<> distr(a->to_real(), b->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct student
{
    inline static const std::string name{ "student-t" };

    inline static const Signature signature{ Double{} };

    inline static const std::string doc{ R"((student-t A)

Return a random real number from a studnet-t distribution with n=A
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_number(a);

        std::student_t_distribution<> distr(a->to_real());

        return make_real(distr(detail::rand_eng));
    }
};

struct seed
{
    inline static const std::string name{ "seed" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((seed INT)

Seed the random engine with the given integer.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        auto a = arg_eval(eval, obj, 0);
        assert_int(a);
        std::seed_seq seed2{ a->to_int() };
        detail::rand_eng = std::mt19937(seed2);

        return Qt;
    }
};

struct seed_rand
{
    inline static const std::string name{ "seed-rand" };

    inline static const Signature signature{ Int{} };

    inline static const std::string doc{ R"((seed-rand)

Seed the random engine with random numbers from the base random device.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
    {
        std::seed_seq seed2{ detail::rand_dev(), detail::rand_dev(), detail::rand_dev(),
                             detail::rand_dev(), detail::rand_dev(), detail::rand_dev() };
        detail::rand_eng = std::mt19937(seed2);
        return Qt;
    }
};

struct crand
{
    inline static const std::string name{ "crand" };

    inline static const Signature signature{};

    inline static const std::string doc{ R"((crand)

Return a random number generated by crand.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *)
    {
        return make_int(std::rand());
    }
};

struct csrand
{
    inline static const std::string name{ "csrand" };

    inline static const Signature signature{ Optional{}, Int{} };

    inline static const std::string doc{ R"((csrand [INT])

Call csrand with the given integer or with the current time if none is provided.
)" };

    static ALObjectPtr func(const ALObjectPtr &obj, env::Environment *, eval::Evaluator *eval)
    {
        assert_max_size<1>(obj);
        if (std::size(*obj) == 0)
        {
            std::srand(static_cast<unsigned int>(std::time(NULL)));
            return Qt;
        }

        auto a = arg_eval(eval, obj, 0);
        assert_int(a);
        std::srand(static_cast<unsigned int>(a->to_int()));
        return Qt;
    }
};

struct module_doc
{
    inline static const std::string doc{ R"(The `random` exposes several functions that have to to with
randomness and the generation thereof. It offers basic RNG as well as
several higher level functions that genrate random numbers from
specific distribution.

Internally `random` uses the C++ facilities for random numbers. Currently the module uses the
[mt19937](https://en.wikipedia.org/wiki/Mersenne_Twister) pseudorandom number generator
providced by the standard c++ library. `random` does, however, provide access to lower level
C-functions like crand.

)" };
};

}  // namespace al_random


ALISP_EXPORT alisp::env::ModulePtr init_random(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    using namespace alisp;

    auto Mrandom  = alisp::module_init("random");
    auto rand_ptr = Mrandom.get();

    module_doc(rand_ptr, al_random::module_doc::doc);

    module_defun(rand_ptr,
                 al_random::rand_int::name,
                 al_random::rand_int::func,
                 al_random::rand_int::doc,
                 al_random::rand_int::signature.al());
    module_defun(rand_ptr,
                 al_random::choice::name,
                 al_random::choice::func,
                 al_random::choice::doc,
                 al_random::choice::signature.al());
    module_defun(rand_ptr,
                 al_random::sample::name,
                 al_random::sample::func,
                 al_random::sample::doc,
                 al_random::sample::signature.al());
    module_defun(rand_ptr,
                 al_random::uniform::name,
                 al_random::uniform::func,
                 al_random::uniform::doc,
                 al_random::uniform::signature.al());
    module_defun(rand_ptr,
                 al_random::exponential::name,
                 al_random::exponential::func,
                 al_random::exponential::doc,
                 al_random::exponential::signature.al());
    module_defun(rand_ptr,
                 al_random::gamma::name,
                 al_random::gamma::func,
                 al_random::gamma::doc,
                 al_random::gamma::signature.al());
    module_defun(rand_ptr,
                 al_random::gauss::name,
                 al_random::gauss::func,
                 al_random::gauss::doc,
                 al_random::gauss::signature.al());
    module_defun(rand_ptr,
                 al_random::lognorm::name,
                 al_random::lognorm::func,
                 al_random::lognorm::doc,
                 al_random::lognorm::signature.al());
    module_defun(rand_ptr,
                 al_random::weibull::name,
                 al_random::weibull::func,
                 al_random::weibull::doc,
                 al_random::weibull::signature.al());
    module_defun(rand_ptr,
                 al_random::geometric::name,
                 al_random::geometric::func,
                 al_random::geometric::doc,
                 al_random::geometric::signature.al());
    module_defun(rand_ptr,
                 al_random::fisher::name,
                 al_random::fisher::func,
                 al_random::fisher::doc,
                 al_random::fisher::signature.al());
    module_defun(rand_ptr,
                 al_random::student::name,
                 al_random::student::func,
                 al_random::student::doc,
                 al_random::student::signature.al());
    module_defun(
      rand_ptr, al_random::seed::name, al_random::seed::func, al_random::seed::doc, al_random::seed::signature.al());
    module_defun(rand_ptr,
                 al_random::seed_rand::name,
                 al_random::seed_rand::func,
                 al_random::seed_rand::doc,
                 al_random::seed_rand::signature.al());
    module_defun(rand_ptr,
                 al_random::crand::name,
                 al_random::crand::func,
                 al_random::crand::doc,
                 al_random::crand::signature.al());
    module_defun(rand_ptr,
                 al_random::csrand::name,
                 al_random::csrand::func,
                 al_random::csrand::doc,
                 al_random::csrand::signature.al());

    return Mrandom;
}
