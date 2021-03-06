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

#include <limits>
#include <cmath>
#include <numeric>

#include "alisp/alisp/alisp_module_helpers.hpp"


namespace alisp
{

namespace detail
{


inline constexpr double PI  = 3.14159265358979323846;
inline constexpr double E   = 2.71828182845904523536;
inline constexpr double TAU = 2 * PI;

inline constexpr auto AL_MAX_INT = std::numeric_limits<ALObject::int_type>::max();
inline constexpr auto AL_MIN_INT = std::numeric_limits<ALObject::int_type>::min();
inline constexpr auto AL_INF_INT = std::numeric_limits<ALObject::int_type>::infinity();

inline constexpr auto AL_MAX_REAL = std::numeric_limits<ALObject::real_type>::max();
inline constexpr auto AL_MIN_REAL = std::numeric_limits<ALObject::real_type>::min();
inline constexpr auto AL_INF_REAL = std::numeric_limits<ALObject::real_type>::infinity();

inline constexpr auto AL_NAN_REAL = NAN;

inline double to_degrees(double t_rad)
{
    return t_rad * 180.0 / PI;
}

inline double to_rad(double t_deg)
{
    return t_deg * PI / 180.0;
}


struct isnan
{
    inline static const std::string name{ "isnan" };

    inline static const std::string doc{ R"((isnan VALUE)

Return `t` if `VALUE` is not a number and `nil` otherwise.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_PREDICATE(Fisnan, std::isnan);
};

struct isinf
{

    inline static const std::string name{ "isinf" };

    inline static const std::string doc{ R"((sin VALUE)

natural logarithm (to base e) of 1 plus the given number
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_PREDICATE(Fisinf, std::isinf);
};

struct todegrees
{
    inline static const std::string name{ "todegrees" };

    inline static const std::string doc{ R"((todegrees VALUE)

Return the `VALUE`(given in radians) in degrees.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ftodegrees, to_degrees);
};

struct toradians
{
    inline static const std::string name{ "toradians" };

    inline static const std::string doc{ R"((toradians VALUE)

Return the `VALUE`(given in degrees) in radians.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ftoradians, to_rad);
};

struct exp
{
    inline static const std::string name{ "exp" };

    inline static const std::string doc{ R"((exp VALUE

Compute the exponential function
))" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fexp, std::exp);
};

struct exp2
{
    inline static const std::string name{ "exp2" };

    inline static const std::string doc{ R"((exp2 VALUE)

Compute binary exponential function
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fexp2, std::exp2);
};

struct expm1
{
    inline static const std::string name{ "expm1" };

    inline static const std::string doc{ R"((expm1 VALUE)

Return e raised to the given power, minus one
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fexpm1, std::expm1);
};

struct log
{
    inline static const std::string name{ "log" };

    inline static const std::string doc{ R"((log VALUE)

Compute natural (base e) logarithm.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Flog, std::log);
};

struct log10
{
    inline static const std::string name{ "log10" };

    inline static const std::string doc{ R"((log10 VALUE)

Compute common (base 10) logarithm.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Flog10, std::log10);
};

struct log2
{
    inline static const std::string name{ "log2" };

    inline static const std::string doc{ R"((log2 VALUE)

Base 2 logarithm of the given number.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Flog2, std::log2);
};

struct log1p
{
    inline static const std::string name{ "log1p" };

    inline static const std::string doc{ R"((log1p VALUE))" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Flog1p, std::log1p);
};

struct sin
{
    inline static const std::string name{ "sin" };
    inline static const std::string doc{ R"((sin VALUE)

Compute sine.
)" };

    static inline const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fsin, std::sin);
};

struct cos
{
    inline static const std::string name{ "cos" };

    inline static const std::string doc{ R"((cos VALUE)

Compute cosine.
 )" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fcos, std::cos);
};

struct tan
{
    inline static const std::string name{ "tan" };

    inline static const std::string doc{ R"((tan VALUE))" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ftan, std::tan);
};

struct asin
{
    inline static const std::string name{ "asin" };

    inline static const std::string doc{ R"((asin VALUE)

Compute arc sine.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fasin, std::asin);
};

struct acos
{
    inline static const std::string name{ "acos" };

    inline static const std::string doc{ R"((acos VALUE)

Compute arc cosine.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Facos, std::acos);
};

struct atan
{
    inline static const std::string name{ "atan" };

    inline static const std::string doc{ R"((atan VALUE)
Compute arc tangent.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fatan, std::atan);
};

struct sinh
{
    inline static const std::string name{ "sinh" };

    inline static const std::string doc{ R"((sinh VALUE)

Compute hyperbolic sine.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fsinh, std::sinh);
};

struct cosh
{
    inline static const std::string name{ "cosh" };

    inline static const std::string doc{ R"((cosh VALUE)

Compute hyperbolic cosine.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fcosh, std::cosh);
};

struct tanh
{
    inline static const std::string name{ "tanh" };

    inline static const std::string doc{ R"((tanh VALUE)

Compute hyperbolic tangent.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ftanh, std::tanh);
};

struct asinh
{
    inline static const std::string name{ "asinh" };

    inline static const std::string doc{ R"((asinh VALUE)

Compute the inverse hyperbolic sine.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fasinh, std::asinh);
};

struct acosh
{
    inline static const std::string name{ "acosh" };

    inline static const std::string doc{ R"((acosh VALUE)

Compute the inverse hyperbolic cosine.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Facosh, std::acosh);
};

struct ceil
{
    inline static const std::string name{ "ceil" };

    inline static const std::string doc{ R"((ceil VALUE)

Return the nearest integer not less than the given value.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fceil, std::ceil);
};

struct floor
{
    inline static const std::string name{ "floor" };

    inline static const std::string doc{ R"((floor VALUE)

Return the nearest integer not greater than the given value.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ffloor, std::floor);
};

struct erf
{
    inline static const std::string name{ "erf" };

    inline static const std::string doc{ R"((erf VALUE)

Compute the error function.)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ferf, std::erf);
};

struct erfc
{
    inline static const std::string name{ "erfc" };

    inline static const std::string doc{ R"((erfc VALUE)

Compute the complementary error function.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ferfc, std::erfc);
};

struct tgamma
{
    inline static const std::string name{ "tgamma" };

    inline static const std::string doc{ R"((tgamma VALUE)

Compute the gamma function.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Ftgamma, std::tgamma);
};

struct lgamma
{
    inline static const std::string name{ "lgamma" };

    inline static const std::string doc{ R"((lgamma VALUE)

Compute the natural logarithm of the gamma function.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Flgamma, std::lgamma);
};

struct hypot
{
    inline static const std::string name{ "hypot" };

    inline static const std::string doc{ R"((hypot VALUE VALUE)

Compute square root of the sum of the squares of two given numbers.
)" };

    inline static const Signature signature{ Number{}, Number{} };

    static REAL_APP_BIFUNCTION(Fhypot, std::hypot);
};

struct pow
{
    inline static const std::string name{ "pow" };

    inline static const std::string doc{ R"((pow VALUE)

Raise a number to the given power.
)" };

    inline static const Signature signature{ Number{}, Number{} };

    static REAL_APP_BIFUNCTION(Fpow, std::pow);
};

struct fdim
{
    inline static const std::string name{ "fdim" };

    inline static const std::string doc{ R"((fdim VALUE)

Compute positive difference of two floating point values.
)" };

    inline static const Signature signature{ Number{}, Number{} };

    static REAL_APP_BIFUNCTION(Ffdim, std::fdim);
};


struct sqrt
{
    inline static const std::string name{ "sqrt" };

    inline static const std::string doc{ R"((sqrt VALUE)

Compute square root.
)" };

    inline static const Signature signature{ Number{}, Number{} };

    static REAL_APP_FUNCTION(Fsqrt, std::sqrt);
};

struct cbrt
{
    inline static const std::string name{ "cbrt" };

    inline static const std::string doc{ R"((cbrt VALUE)

Compute cubic root.
)" };

    inline static const Signature signature{ Number{} };

    static REAL_APP_FUNCTION(Fcbrt, std::cbrt);
};


struct gcd
{
    inline static const std::string name{ "gcd" };

    inline static const std::string doc{ R"((gcd VALUE)

Compute greatest common denominator
)" };

    inline static const Signature signature{ Int{}, Int{} };

    static INT_APP_BIFUNCTION(Fgcd, std::gcd);
};

struct lcm
{
    inline static const std::string name{ "lcm" };

    inline static const std::string doc{ R"((lcm VALUE)

Compute lowest common denominator
)" };

    inline static const Signature signature{ Int{}, Int{} };

    static INT_APP_BIFUNCTION(Flcm, std::lcm);
};


struct pi_var
{

    static inline const std::string name{ "PI" };

    static inline const std::string doc{ R"(The value of Pi (3.14159265....))" };

    static inline const auto var = make_double(detail::PI);
};

struct e_var
{
    static inline const std::string name{ "E" };

    static inline const std::string doc{ R"(The value of E (2.71828...))" };

    static inline const auto var = make_double(detail::E);
};

struct tau_var
{
    static inline const std::string name{ "TAU" };

    static inline const std::string doc{ R"(The value of Tau (2xPi))" };

    static inline const auto var = make_double(detail::TAU);
};

struct max_int_var
{
    static inline const std::string name{ "TAU" };

    static inline const std::string doc{ R"(The maximal value that an integer can take.)" };

    static inline const auto var = make_double(detail::AL_MAX_INT);
};

struct min_int_var
{
    static inline const std::string name{ "TAU" };

    static inline const std::string doc{ R"(The min value that an integer can take.)" };

    static inline const auto var = make_double(detail::AL_MIN_INT);
};

struct inf_var
{
    static inline const std::string name{ "TAU" };

    static inline const std::string doc{ R"(Value used to represent infinity)" };

    static inline const auto var = make_double(detail::AL_INF_INT);
};

struct max_real_var
{
    static inline const std::string name{ "TAU" };

    static inline const std::string doc{ R"(The maximal value that a real number can take.)" };

    static inline const auto var = make_double(detail::AL_MAX_REAL);
};

struct min_real_var
{
    static inline const std::string name{ "TAU" };

    static inline const std::string doc{ R"(The minimal value that a real number can take.)" };

    static inline const auto var = make_double(detail::AL_MIN_REAL);
};


struct module_doc
{
    inline static const std::string doc{ R"(The `math` provides more complicated math functions. Often these
function are just wrappers around the standard C++ functions.
)" };
};

}  // namespace detail

env::ModulePtr init_math(env::Environment *, eval::Evaluator *)
{

    auto Mmath    = module_init("math");
    auto math_ptr = Mmath.get();

    module_doc(math_ptr, detail::module_doc::doc);


    using namespace detail;
    module_defun(math_ptr, isnan::name, isnan::Fisnan, isnan::doc, isnan::signature.al());
    module_defun(math_ptr, isinf::name, isinf::Fisinf, isinf::doc, isinf::signature.al());
    module_defun(math_ptr, todegrees::name, todegrees::Ftodegrees, todegrees::doc, todegrees::signature.al());
    module_defun(math_ptr, toradians::name, toradians::Ftoradians, toradians::doc, toradians::signature.al());
    module_defun(math_ptr, exp::name, exp::Fexp, exp::doc, exp::signature.al());
    module_defun(math_ptr, exp2::name, exp2::Fexp2, exp2::doc, exp2::signature.al());
    module_defun(math_ptr, expm1::name, expm1::Fexpm1, expm1::doc, expm1::signature.al());
    module_defun(math_ptr, log::name, log::Flog, log::doc, log::signature.al());
    module_defun(math_ptr, log10::name, log10::Flog10, log10::doc, log10::signature.al());
    module_defun(math_ptr, log2::name, log2::Flog2, log2::doc, log2::signature.al());
    module_defun(math_ptr, log1p::name, log1p::Flog1p, log1p::doc, log1p::signature.al());
    module_defun(math_ptr, sin::name, sin::Fsin, sin::doc, sin::signature.al());
    module_defun(math_ptr, cos::name, cos::Fcos, cos::doc, cos::signature.al());
    module_defun(math_ptr, tan::name, tan::Ftan, tan::doc, tan::signature.al());
    module_defun(math_ptr, asin::name, asin::Fasin, asin::doc, asin::signature.al());
    module_defun(math_ptr, acos::name, acos::Facos, acos::doc, acos::signature.al());
    module_defun(math_ptr, atan::name, atan::Fatan, atan::doc, atan::signature.al());
    module_defun(math_ptr, sinh::name, sinh::Fsinh, sinh::doc, sinh::signature.al());
    module_defun(math_ptr, cosh::name, cosh::Fcosh, cosh::doc, cosh::signature.al());
    module_defun(math_ptr, tanh::name, tanh::Ftanh, tanh::doc, tanh::signature.al());
    module_defun(math_ptr, asinh::name, asinh::Fasinh, asinh::doc, asinh::signature.al());
    module_defun(math_ptr, acosh::name, acosh::Facosh, acosh::doc, acosh::signature.al());
    module_defun(math_ptr, ceil::name, ceil::Fceil, ceil::doc, ceil::signature.al());
    module_defun(math_ptr, floor::name, floor::Ffloor, floor::doc, floor::signature.al());
    module_defun(math_ptr, erf::name, erf::Ferf, erf::doc, erf::signature.al());
    module_defun(math_ptr, erfc::name, erfc::Ferfc, erfc::doc, erfc::signature.al());
    module_defun(math_ptr, tgamma::name, tgamma::Ftgamma, tgamma::doc, tgamma::signature.al());
    module_defun(math_ptr, lgamma::name, lgamma::Flgamma, lgamma::doc, lgamma::signature.al());
    module_defun(math_ptr, hypot::name, hypot::Fhypot, hypot::doc, hypot::signature.al());
    module_defun(math_ptr, pow::name, pow::Fpow, pow::doc, pow::signature.al());
    module_defun(math_ptr, fdim::name, fdim::Ffdim, fdim::doc, fdim::signature.al());
    module_defun(math_ptr, sqrt::name, sqrt::Fsqrt, sqrt::doc, sqrt::signature.al());
    module_defun(math_ptr, cbrt::name, cbrt::Fcbrt, cbrt::doc, cbrt::signature.al());
    module_defun(math_ptr, gcd::name, gcd::Fgcd, gcd::doc, gcd::signature.al());
    module_defun(math_ptr, lcm::name, lcm::Flcm, lcm::doc, lcm::signature.al());


    return Mmath;
}


}  // namespace alisp
