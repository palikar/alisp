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


REAL_APP_PREDICATE(Fisnan, std::isnan);
REAL_APP_PREDICATE(Fisinf, std::isinf);

REAL_APP_FUNCTION(Ftodegrees, to_degrees);
REAL_APP_FUNCTION(Ftoradians, to_rad);

REAL_APP_FUNCTION(Fexp, std::exp);
REAL_APP_FUNCTION(Fexp2, std::exp2);
REAL_APP_FUNCTION(Fexpm1, std::expm1);
REAL_APP_FUNCTION(Flog, std::log);
REAL_APP_FUNCTION(Flog10, std::log10);
REAL_APP_FUNCTION(Flog2, std::log2);
REAL_APP_FUNCTION(Flog1p, std::log1p);
REAL_APP_FUNCTION(Fsin, std::sin);
REAL_APP_FUNCTION(Fcos, std::cos);
REAL_APP_FUNCTION(Ftan, std::tan);
REAL_APP_FUNCTION(Fasin, std::asin);
REAL_APP_FUNCTION(Facos, std::acos);
REAL_APP_FUNCTION(Fatan, std::atan);
REAL_APP_FUNCTION(Fsinh, std::sinh);
REAL_APP_FUNCTION(Fcosh, std::cosh);
REAL_APP_FUNCTION(Ftanh, std::tanh);
REAL_APP_FUNCTION(Fasinh, std::asinh);
REAL_APP_FUNCTION(Facosh, std::acosh);
REAL_APP_FUNCTION(Fceil, std::ceil);
REAL_APP_FUNCTION(Ffloor, std::floor);
REAL_APP_FUNCTION(Ferf, std::erf);
REAL_APP_FUNCTION(Ferfc, std::erfc);
REAL_APP_FUNCTION(Ftgamma, std::tgamma);
REAL_APP_FUNCTION(Flgamma, std::lgamma);
REAL_APP_BIFUNCTION(Fhypot, std::hypot);
REAL_APP_BIFUNCTION(Fpow, std::pow);
REAL_APP_BIFUNCTION(Ffdim, std::fdim);


REAL_APP_FUNCTION(Fsqrt, std::sqrt);
REAL_APP_FUNCTION(Fcbrt, std::cbrt);

INT_APP_BIFUNCTION(Fgcd, std::gcd);
INT_APP_BIFUNCTION(Flcm, std::lcm);

}  // namespace detail

env::ModulePtr init_math(env::Environment *, eval::Evaluator *)
{

    auto Mmath    = module_init("math");
    auto math_ptr = Mmath.get();

    module_doc(math_ptr,
               R"(The `math` provides more complicated math functions. Often these
function are just wrappers around the standard C++ functions.
)");

    module_defvar(math_ptr, "PI", make_double(detail::PI), R"(The value of Pi (3.14159265....))");

    module_defvar(math_ptr, "E", make_double(detail::E), R"(The value of E (2.71828...))");

    module_defvar(math_ptr, "TAU", make_double(detail::TAU), R"(The value of Tau (2xPi))");


    module_defvar(math_ptr, "max-int", make_int(detail::AL_MAX_INT), R"(The maximal value that an integer can take.)");

    module_defvar(math_ptr, "min-int", make_int(detail::AL_MIN_INT), R"(The min value that an integer can take.)");

    module_defvar(math_ptr, "inf-int", make_int(detail::AL_INF_INT), R"(Value used to represent infinity)");

    module_defvar(
      math_ptr, "max-real", make_real(detail::AL_MAX_REAL), R"(The maximal value that a real number can take.)");

    module_defvar(
      math_ptr, "min-real", make_real(detail::AL_MIN_REAL), R"(The minimal value that a real number can take.)");


    module_defun(math_ptr,
                 "isinf",
                 &detail::Fisinf,
                 R"((isinf VALUE)

Return `t` if `VALUE` is infinity and `nil` otherwise.
)");

    module_defun(math_ptr,
                 "isnan",
                 &detail::Fisnan,
                 R"((isnan VALUE)

Return `t` if `VALUE` is not a number and `nil` otherwise.
)");


    module_defun(math_ptr,
                 "todegrees",
                 &detail::Ftodegrees,
                 R"((todegrees VALUE)

Return the `VALUE`(given in radians) in degrees.
)");

    module_defun(math_ptr,
                 "toradians",
                 &detail::Ftoradians,
                 R"((toradians VALUE)

Return the `VALUE`(given in degrees) in radians.
)");


    module_defun(math_ptr,
                 "exp",
                 &detail::Fexp,
                 R"((exp VALUE)

Compute exponential function.
)");

    module_defun(math_ptr,
                 "exp2",
                 &detail::Fexp2,
                 R"((exp2 VALUE)

Compute binary exponential function
)");

    module_defun(math_ptr,
                 "expm1",
                 &detail::Fexpm1,
                 R"((expm1 VALUE)

Return e raised to the given power, minus one
)");

    module_defun(math_ptr,
                 "log",
                 &detail::Flog,
                 R"((log VALUE)

Compute natural (base e) logarithm.
)");

    module_defun(math_ptr,
                 "log10",
                 &detail::Flog10,
                 R"((log10 VALUE)

Compute common (base 10) logarithm.
)");

    module_defun(math_ptr,
                 "log2",
                 &detail::Flog2,
                 R"((log2 VALUE)

Base 2 logarithm of the given number.
)");

    module_defun(math_ptr, "log1p", &detail::Flog1p, R"((log1p VALUE))");

    module_defun(math_ptr,
                 "sin",
                 &detail::Fsin,
                 R"((sin VALUE)

natural logarithm (to base e) of 1 plus the given number
)");

    module_defun(math_ptr,
                 "cos",
                 &detail::Fcos,
                 R"((cos VALUE)

Compute cosine.
 )");

    module_defun(math_ptr,
                 "tan",
                 &detail::Ftan,
                 R"((tan VALUE)

Compute tangent.
)");

    module_defun(math_ptr,
                 "asin",
                 &detail::Fasin,
                 R"((asin VALUE)

Compute arc sine.
)");

    module_defun(math_ptr,
                 "acos",
                 &detail::Facos,
                 R"((acos VALUE)

Compute arc cosine.
)");

    module_defun(math_ptr,
                 "atan",
                 &detail::Fatan,
                 R"((atan VALUE)
Compute arc tangent.
)");

    module_defun(math_ptr,
                 "sinh",
                 &detail::Fsinh,
                 R"((sinh VALUE)

Compute hyperbolic sine.
)");

    module_defun(math_ptr,
                 "cosh",
                 &detail::Fcosh,
                 R"((cosh VALUE)

Compute hyperbolic cosine.
)");

    module_defun(math_ptr,
                 "tanh",
                 &detail::Ftanh,
                 R"((tanh VALUE)

Compute hyperbolic tangent.
)");

    module_defun(math_ptr,
                 "asinh",
                 &detail::Fasinh,
                 R"((asinh VALUE)

Compute the inverse hyperbolic sine.
)");

    module_defun(math_ptr,
                 "acosh",
                 &detail::Facosh,
                 R"((acosh VALUE)

Compute the inverse hyperbolic cosine.
)");

    module_defun(math_ptr,
                 "ceil",
                 &detail::Fceil,
                 R"((ceil VALUE)

Return the nearest integer not less than the given value.
)");

    module_defun(math_ptr,
                 "floor",
                 &detail::Ffloor,
                 R"((floor VALUE)

Return the nearest integer not greater than the given value.
)");

    module_defun(math_ptr,
                 "erf",
                 &detail::Ferf,
                 R"((erf VALUE)

Compute the error function.)");

    module_defun(math_ptr,
                 "erfc",
                 &detail::Ferfc,
                 R"((erfc VALUE)

Compute the complementary error function.
)");

    module_defun(math_ptr,
                 "tgamma",
                 &detail::Ftgamma,
                 R"((tgamma VALUE)

Compute the gamma function.
)");

    module_defun(math_ptr,
                 "lgamma",
                 &detail::Flgamma,
                 R"((lgamma VALUE)

Compute the natural logarithm of the gamma function.
)");

    module_defun(math_ptr,
                 "fdim",
                 &detail::Ffdim,
                 R"((fdim VALUE)

Compute positive difference of two floating point values.
)");

    module_defun(math_ptr,
                 "pow",
                 &detail::Fpow,
                 R"((pow VALUE)

Raise a number to the given power.
)");

    module_defun(math_ptr,
                 "sqrt",
                 &detail::Fsqrt,
                 R"((sqrt VALUE)

Compute square root.
)");

    module_defun(math_ptr,
                 "cbrt",
                 &detail::Fcbrt,
                 R"((cbrt VALUE)

Compute cubic root.
)");

    module_defun(math_ptr,
                 "hypot",
                 &detail::Fhypot,
                 R"((hypot VALUE)

Compute square root of the sum of the squares of two given numbers.
)");


    return Mmath;
}


}  // namespace alisp
