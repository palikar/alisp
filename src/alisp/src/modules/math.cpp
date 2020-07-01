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
    inline static const std::string name{"isnan"};

    inline static const std::string doc{R"((isnan VALUE)

Return `t` if `VALUE` is not a number and `nil` otherwise.
)"};

    static REAL_APP_PREDICATE(Fisnan, std::isnan);
};

struct isinf
{

    inline static const std::string name{"isinf"};

    inline static const std::string doc{R"((sin VALUE)

natural logarithm (to base e) of 1 plus the given number
)"};

    inline static const std::string doc{R"((isinf VALUE)

Return `t` if `VALUE` is infinity and `nil` otherwise.
)"};

    static REAL_APP_PREDICATE(Fisinf, std::isinf);
};


struct todegrees
{
    inline static const std::string name{"todegrees"};

    inline static const std::string doc{R"((todegrees VALUE)

Return the `VALUE`(given in radians) in degrees.
)"};

    static REAL_APP_FUNCTION(Ftodegrees, to_degrees);
};

struct toradians
{
    inline static const std::string name{"toradians"};

    inline static const std::string doc{R"((toradians VALUE)

Return the `VALUE`(given in degrees) in radians.
)"};

    static REAL_APP_FUNCTION(Ftoradians, to_rad);
};

struct exp
{
    inline static const std::string name{"exp"};

    inline static const std::string doc{R"((exp VALUE

Compute the exponential function
))"};
    
    static REAL_APP_FUNCTION(Fexp, std::exp);
};

struct exp2
{
    inline static const std::string name{"exp2"};

    inline static const std::string doc{R"((exp2 VALUE)

Compute binary exponential function
)"};

    static REAL_APP_FUNCTION(Fexp2, std::exp2);
};

struct expm1
{
    inline static const std::string name{"expm1"};

    inline static const std::string doc{R"((expm1 VALUE)

Return e raised to the given power, minus one
)"};

    static REAL_APP_FUNCTION(Fexpm1, std::expm1);
};

struct log
{
    inline static const std::string name{"log"};

    inline static const std::string doc{R"((log VALUE)

Compute natural (base e) logarithm.
)"};

    static REAL_APP_FUNCTION(Flog, std::log);
};

struct log10
{
    inline static const std::string name{"log10"};

    inline static const std::string doc{R"((log10 VALUE)

Compute common (base 10) logarithm.
)"};

    static REAL_APP_FUNCTION(Flog10, std::log10);
};

struct log2
{
    inline static const std::string name{"log2"};

    inline static const std::string doc{R"((log2 VALUE)

Base 2 logarithm of the given number.
)"};

    static REAL_APP_FUNCTION(Flog2, std::log2);
};

struct log1p
{
    inline static const std::string name{"log1p"};

    inline static const std::string doc{R"((log1p VALUE))"};

    static REAL_APP_FUNCTION(Flog1p, std::log1p);
};

struct sin
{
    inline static const std::string name{"sin"};

    static REAL_APP_FUNCTION(Fsin, std::sin);
};

struct cos
{
    inline static const std::string name{"cos"};

    inline static const std::string doc{R"((cos VALUE)

Compute cosine.
 )"};

    static REAL_APP_FUNCTION(Fcos, std::cos);
};

struct tan
{
    inline static const std::string name{"tan"};

    static REAL_APP_FUNCTION(Ftan, std::tan);
};

struct asin
{
    inline static const std::string name{"asin"};

    inline static const std::string doc{R"((asin VALUE)

Compute arc sine.
)"};

    static REAL_APP_FUNCTION(Fasin, std::asin);
};

struct acos
{
    inline static const std::string name{"acos"};

    inline static const std::string doc{R"((acos VALUE)

Compute arc cosine.
)"};

    static REAL_APP_FUNCTION(Facos, std::acos);
};

struct atan
{
    inline static const std::string name{"atan"};

    inline static const std::string doc{R"((atan VALUE)
Compute arc tangent.
)"};

    static REAL_APP_FUNCTION(Fatan, std::atan);
};

struct sinh
{
    inline static const std::string name{"sinh"};

    inline static const std::string doc{R"((sinh VALUE)

Compute hyperbolic sine.
)"};

    static REAL_APP_FUNCTION(Fsinh, std::sinh);
};

struct cosh
{
    inline static const std::string name{"cosh"};

    inline static const std::string doc{R"((cosh VALUE)

Compute hyperbolic cosine.
)"};

    static REAL_APP_FUNCTION(Fcosh, std::cosh);
};

struct tanh
{
    inline static const std::string name{"tanh"};

    inline static const std::string doc{R"((tanh VALUE)

Compute hyperbolic tangent.
)"};

    static REAL_APP_FUNCTION(Ftanh, std::tanh);
};

struct asinh
{
    inline static const std::string name{"asinh"};

    inline static const std::string doc{R"((asinh VALUE)

Compute the inverse hyperbolic sine.
)"};

    static REAL_APP_FUNCTION(Fasinh, std::asinh);
};

struct acosh
{
    inline static const std::string name{"acosh"};

    inline static const std::string doc{R"((acosh VALUE)

Compute the inverse hyperbolic cosine.
)"};

    static REAL_APP_FUNCTION(Facosh, std::acosh);
};

struct ceil
{
    inline static const std::string name{"ceil"};

    inline static const std::string doc{R"((ceil VALUE)

Return the nearest integer not less than the given value.
)"};

    static REAL_APP_FUNCTION(Fceil, std::ceil);
};

struct floor
{
    inline static const std::string name{"floor"};

    inline static const std::string doc{R"((floor VALUE)

Return the nearest integer not greater than the given value.
)"};

    static REAL_APP_FUNCTION(Ffloor, std::floor);
};

struct erf
{
    inline static const std::string name{"erf"};

    inline static const std::string doc{R"((erf VALUE)

Compute the error function.)"};

    static REAL_APP_FUNCTION(Ferf, std::erf);
};

struct erfc
{
    inline static const std::string name{"erfc"};

    inline static const std::string doc{R"((erfc VALUE)

Compute the complementary error function.
)"};

    static REAL_APP_FUNCTION(Ferfc, std::erfc);
};

struct tgamma
{
    inline static const std::string name{"tgamma"};

    inline static const std::string doc{R"((tgamma VALUE)

Compute the gamma function.
)"};

    static REAL_APP_FUNCTION(Ftgamma, std::tgamma);
};

struct lgamma
{
    inline static const std::string name{"lgamma"};

    inline static const std::string doc{R"((lgamma VALUE)

Compute the natural logarithm of the gamma function.
)"};

    static REAL_APP_FUNCTION(Flgamma, std::lgamma);
};

struct hypot
{
    inline static const std::string name{"hypot"};

    inline static const std::string doc{R"((hypot VALUE)

Compute square root of the sum of the squares of two given numbers.
)"};

    static REAL_APP_BIFUNCTION(Fhypot, std::hypot);
};

struct pow
{
    inline static const std::string name{"pow"};

    inline static const std::string doc{R"((pow VALUE)

Raise a number to the given power.
)"};

    static REAL_APP_BIFUNCTION(Fpow, std::pow);
};

struct fdim
{
    inline static const std::string name{"fdim"};

    inline static const std::string doc{R"((fdim VALUE)

Compute positive difference of two floating point values.
)"};

    static REAL_APP_BIFUNCTION(Ffdim, std::fdim);
};



struct sqrt
{
    inline static const std::string name{"sqrt"};

    inline static const std::string doc{R"((sqrt VALUE)

Compute square root.
)"};

    static REAL_APP_FUNCTION(Fsqrt, std::sqrt);
};

struct cbrt
{
    inline static const std::string name{"cbrt"};

    inline static const std::string doc{R"((cbrt VALUE)

Compute cubic root.
)"};

    static REAL_APP_FUNCTION(Fcbrt, std::cbrt);
};


struct gcd
{
    inline static const std::string name{"gcd"};

    inline static const std::string doc{R"((gcd VALUE)

Compute greatest common denominator
)"};

    static INT_APP_BIFUNCTION(Fgcd, std::gcd);
};

struct lcm
{
    inline static const std::string name{"lcm"};

    inline static const std::string doc{R"((lcm VALUE)

Compute lowest common denominator
)"};

    static INT_APP_BIFUNCTION(Flcm, std::lcm);
};


struct pi_var
{

    static inline const std::string name{"PI"};

    static inline const std::string doc{R"((isinf VALUE)

Return `t` if `VALUE` is infinity and `nil` otherwise.
)"};

    static inline const std::string doc{R"(The value of Pi (3.14159265....))"};

    static inline const auto var = make_double(details::PI);
};

struct e_var
{
    static inline const std::string name{"E"};

    static inline const std::string doc{R"(The value of E (2.71828...))"};

    static inline const auto var = make_double(details::E);
};

struct tau_var
{
    static inline const std::string name{"TAU"};

    static inline const std::string doc{R"(The value of Tau (2xPi))"};

    static inline const auto var = make_double(details::TAU);
};

struct max_int_var
{
    static inline const std::string name{"TAU"};

    static inline const std::string doc{R"(The maximal value that an integer can take.)"};

    static inline const auto var = make_double(detail::AL_MAX_INT);
};

struct min_int_var
{
    static inline const std::string name{"TAU"};

    static inline const std::string doc{R"(The min value that an integer can take.)"};

    static inline const auto var = make_double(detail::AL_MIN_INT);
};

struct inf_var
{
    static inline const std::string name{"TAU"};

    static inline const std::string doc{R"(Value used to represent infinity)"};

    static inline const auto var = make_double(detail::AL_INF_INT);
};

struct max_real_var
{
    static inline const std::string name{"TAU"};

    static inline const std::string doc{R"(The maximal value that a real number can take.)"};

    static inline const auto var = make_double(detail::AL_MAX_REAL);
};

struct min_real_var
{
    static inline const std::string name{"TAU"};

    static inline const std::string doc{R"(The minimal value that a real number can take.)"};

    static inline const auto var = make_double(detail::AL_MIN_REAL);
};



struct module_doc
{
    inline static const std::string doc{R"(The `math` provides more complicated math functions. Often these
function are just wrappers around the standard C++ functions.
)"};
    
};

}  // namespace detail

env::ModulePtr init_math(env::Environment *, eval::Evaluator *)
{

    auto Mmath    = module_init("math");
    auto math_ptr = Mmath.get();


    module_doc(math_ptr, detail::module_doc::doc);

    return Mmath;
}


}  // namespace alisp
