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

ALObjectPtr Frand_int(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_int(a);
    auto b = eval->eval(obj->i(1));
    assert_int(b);

    std::uniform_int_distribution<> distr(static_cast<int>(a->to_int()), static_cast<int>(b->to_int()) + 1);

    return make_int(distr(detail::rand_eng));
}

ALObjectPtr Fchoice(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_list(a);

    std::uniform_int_distribution<> distr(0, static_cast<int>(a->size()) - 1);

    return a->children()[static_cast<size_t>(distr(detail::rand_eng))];
}

ALObjectPtr Fsample(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_list(a);
    auto k = eval->eval(obj->i(1));
    assert_int(k);
    std::uniform_int_distribution<> distr(0, static_cast<int>(a->size()) - 1);
    ALObject::list_type lis;
    for (int i = 0; i < k->to_int(); ++i)
    {
        lis.push_back(a->children()[static_cast<size_t>(distr(detail::rand_eng))]);
    }

    return make_list(lis);
}

ALObjectPtr Funiform(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);
    auto b = eval->eval(obj->i(1));
    assert_number(b);

    std::uniform_real_distribution<> distr(a->to_real(), b->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fexponential(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);

    std::exponential_distribution<> distr(a->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fgamma(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);
    auto b = eval->eval(obj->i(1));
    assert_number(b);

    std::gamma_distribution<> distr(a->to_real(), b->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fgauss(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);
    auto b = eval->eval(obj->i(1));
    assert_number(b);

    std::normal_distribution<> distr(a->to_real(), b->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Flognorm(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);
    auto b = eval->eval(obj->i(1));
    assert_number(b);

    std::lognormal_distribution<> distr(a->to_real(), b->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fweibull(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);
    auto b = eval->eval(obj->i(1));
    assert_number(b);

    std::weibull_distribution<> distr(a->to_real(), b->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fgeometric(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);

    std::geometric_distribution<> distr(a->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Ffisher(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);
    auto b = eval->eval(obj->i(1));
    assert_number(b);

    std::fisher_f_distribution<> distr(a->to_real(), b->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fstudent(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_number(a);

    std::student_t_distribution<> distr(a->to_real());

    return make_real(distr(detail::rand_eng));
}

ALObjectPtr Fseed(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_int(a);
    std::seed_seq seed2{ a->to_int() };
    detail::rand_eng = std::mt19937(seed2);

    return Qt;
}

ALObjectPtr Fseed_rand(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    assert_size<0>(obj);
    std::seed_seq seed2{ detail::rand_dev(), detail::rand_dev(), detail::rand_dev(),
                         detail::rand_dev(), detail::rand_dev(), detail::rand_dev() };
    detail::rand_eng = std::mt19937(seed2);
    return Qt;
}

ALObjectPtr Fcrand(ALObjectPtr obj, env::Environment *, eval::Evaluator *)
{
    assert_size<0>(obj);
    return make_int(std::rand());
}

ALObjectPtr Fcsrand(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_max_size<1>(obj);
    if (std::size(*obj) == 0)
    {
        std::srand(static_cast<unsigned int>(std::time(NULL)));
        return Qt;
    }

    auto a = eval->eval(obj->i(0));
    assert_int(a);
    std::srand(static_cast<unsigned int>(a->to_int()));
    return Qt;
}


}  // namespace al_random


ALISP_EXPORT alisp::env::ModulePtr init_random(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mrandom  = alisp::module_init("random");
    auto rand_ptr = Mrandom.get();


    alisp::module_doc(rand_ptr, R"(The `random` exposes several functions that have to to with
randomness and the generation thereof. It offers basic RNG as well as
several higher level functions that genrate random numbers from
specific distribution.

Internally `random` uses the C++ facilities for random numbers. Currently the module uses the
[mt19937](https://en.wikipedia.org/wiki/Mersenne_Twister) pseudorandom number generator
providced by the standard c++ library. `random` does, however, provide access to lower level
C-functions like crand.

)");

    alisp::module_defun(rand_ptr,
                        "seed",
                        &al_random::Fseed,
                        R"((seed INT)

Seed the random engine with the given integer.
)");

    alisp::module_defun(rand_ptr,
                        "seed-rand",
                        &al_random::Fseed_rand,
                        R"((seed-rand)

Seed the random engine with random numbers from the base random device.
)");


    alisp::module_defun(rand_ptr,
                        "crand",
                        &al_random::Fcrand,
                        R"((crand)

Return a random number generated by crand.
)");

    alisp::module_defun(rand_ptr,
                        "csrand",
                        &al_random::Fcsrand,
                        R"((csrand [INT])

Call csrand with the given integer or with the current time if none is provided.
)");


    alisp::module_defun(rand_ptr,
                        "rand-int",
                        &al_random::Frand_int,
                        R"((rand-int LOWER UPPER)

Return a random integer in the range [LOWER, UPPER]
)");

    alisp::module_defun(rand_ptr,
                        "choice",
                        &al_random::Fchoice,
                        R"((choice LIST)

Return a random element from the list `LIST`.
)");

    alisp::module_defun(rand_ptr,
                        "sample",
                        &al_random::Fsample,
                        R"((choice LIST CNT)

Return a list with `CNT` random elements from the list `LIST`.

)");


    alisp::module_defun(rand_ptr,
                        "uniform",
                        &al_random::Funiform,
                        R"((uniform A B)

Return a random real number from an uniform distribution of [A, B].
)");

    alisp::module_defun(rand_ptr,
                        "exponential",
                        &al_random::Fexponential,
                        R"((exponential A)

Return a random real number from an exponential distribution with e=A
)");

    alisp::module_defun(rand_ptr,
                        "gamma",
                        &al_random::Fgamma,
                        R"((gamma A B)

Return a random real number from a gamma distribution with k=A, theta=B
)");

    alisp::module_defun(rand_ptr,
                        "gauss",
                        &al_random::Fgauss,
                        R"((gauss A B)

Return a random real number from a gauss distribution with mean=A, std=B
)");

    alisp::module_defun(rand_ptr,
                        "lognorm",
                        &al_random::Flognorm,
                        R"((lognorm A)

Return a random real number from a log-normal distribution with mean=A, std=B
)");

    alisp::module_defun(rand_ptr,
                        "weibull",
                        &al_random::Fweibull,
                        R"((weibull A B)

Return a random real number from a weibull distribution with lambda=A, k=B
)");

    alisp::module_defun(rand_ptr,
                        "student-t",
                        &al_random::Fstudent,
                        R"((student-t A)

Return a random real number from a studnet-t distribution with n=A
)");

    alisp::module_defun(rand_ptr,
                        "fisher-f",
                        &al_random::Ffisher,
                        R"((fisher-f A B)

Return a random real number from a f-distribution distribution with m=A, n=B
)");

    alisp::module_defun(rand_ptr,
                        "geometric",
                        &al_random::Fgeometric,
                        R"((geometric A)

Return a random real number from a geometric distribution with p=A
)");


    return Mrandom;
}
