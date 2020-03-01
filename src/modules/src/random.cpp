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
    { lis.push_back(a->children()[static_cast<size_t>(distr(detail::rand_eng))]); }

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
    assert_number(a);

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


    alisp::module_doc(rand_ptr, R"()");

    alisp::module_defun(rand_ptr, "seed", &al_random::Fseed);
    alisp::module_defun(rand_ptr, "seed-rand", &al_random::Fseed_rand);

    alisp::module_defun(rand_ptr, "crand", &al_random::Fcrand);
    alisp::module_defun(rand_ptr, "csrand", &al_random::Fcsrand);

    alisp::module_defun(rand_ptr, "rand-int", &al_random::Frand_int);
    alisp::module_defun(rand_ptr, "choice", &al_random::Fchoice);
    alisp::module_defun(rand_ptr, "sample", &al_random::Fsample);

    alisp::module_defun(rand_ptr, "uniform", &al_random::Funiform);
    alisp::module_defun(rand_ptr, "expovariate", &al_random::Fexponential);
    alisp::module_defun(rand_ptr, "gammavariate", &al_random::Fgamma);
    alisp::module_defun(rand_ptr, "gauss", &al_random::Fgauss);
    alisp::module_defun(rand_ptr, "lognormvariate", &al_random::Flognorm);
    alisp::module_defun(rand_ptr, "weibullvariate", &al_random::Fweibull);
    alisp::module_defun(rand_ptr, "student-t", &al_random::Fstudent);
    alisp::module_defun(rand_ptr, "fisher-f", &al_random::Ffisher);
    alisp::module_defun(rand_ptr, "geometric", &al_random::Fgeometric);


    return Mrandom;
}
