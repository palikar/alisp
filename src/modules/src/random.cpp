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


}


ALObjectPtr Frand_int(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<2>(obj);
    auto a = eval->eval(obj->i(0));
    assert_int(a);
    auto b = eval->eval(obj->i(1));
    assert_int(b);

    std::uniform_int_distribution<> distr(a->to_int(), b->to_int()+1);

    return make_int(distr(detail::rand_eng));
}


ALObjectPtr Fchoice(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_list(a);
    
    std::uniform_int_distribution<> distr(0, a->size() - 1);
    
    return a->children()[distr(detail::rand_eng)];
}


ALObjectPtr Fsample(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    auto a = eval->eval(obj->i(0));
    assert_list(a);
    auto k = eval->eval(obj->i(1));
    assert_int(k);
    std::uniform_int_distribution<> distr(0, a->size() - 1);
    ALObject::list_type lis;
    for (int i = 0; i < k->to_int(); ++i) {
        lis.push_back(a->children()[distr(detail::rand_eng)]);
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
    assert_size<2>(obj);
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
    assert_size<2>(obj);
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
    assert_size<2>(obj);
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


ALObjectPtr Fseed_rand(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<0>(obj);
    

    return Qt;
}

ALObjectPtr Fcrand(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<0>(obj);
    

    return Qt;
}

ALObjectPtr Fcsrand(ALObjectPtr obj, env::Environment *, eval::Evaluator *eval)
{
    assert_size<1>(obj);
    

    return Qt;
}


}


ALISP_EXPORT alisp::env::ModulePtr init_random(alisp::env::Environment *, alisp::eval::Evaluator *)
{
    auto Mrandom = alisp::module_init("random");
    auto rand_ptr = Mrandom.get();

    // alisp::module_defun(xml_ptr, "xml-parse", &Fparse_xml);

    alisp::module_doc(rand_ptr, R"()");

    // alisp::module_defvar(rand_ptr, "seed", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "seed-rand", rand::json_signal);
    
    // alisp::module_defvar(rand_ptr, "rand-int", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "choice", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "sample", rand::json_signal);

    // alisp::module_defvar(rand_ptr, "random", rand::json_signal);
    
    // alisp::module_defvar(rand_ptr, "uniform", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "betavariate", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "expovariate", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "gammavariate", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "gauss", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "lognormvariate", rand::json_signal);
    // alisp::module_defvar(rand_ptr, "weibullvariate", rand::json_signal);


    return Mrandom;
}
