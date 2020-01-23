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

#pragma once

#include "alisp/config.hpp"
#include <fmt/format.h>

#include <vector>

#define ALISP_COMPILER_VERSION __VERSION__

#if defined(_WIN32)
#if defined(__llvm__)
#define ALISP_COMPILER_NAME "clang(windows)"
#elif defined(__GNUC__)
#define ALISP_COMPILER_NAME "gcc(mingw)"
#else
#define ALISP_COMPILER_NAME "msvc"
#endif
#else
#if defined(__llvm__)
#define ALISP_COMPILER_NAME "clang"
#elif defined(__GNUC__)
#define ALISP_COMPILER_NAME "gcc"
#else
#define ALISP_COMPILER_NAME "unknown"
#endif
#endif


#ifndef NDEBUG
#define ALISP_DEBUG true
#else
#define ALISP_DEBUG false
#endif


namespace alisp
{

constexpr static const int version_major = AL_VERSION_MAJOR;
constexpr static const int version_minor = AL_VERSION_MINOR;
constexpr static const int version_patch = AL_VERSION_PATCH;

constexpr static const char *compiler_version = ALISP_COMPILER_VERSION;
constexpr static const char *compiler_name = ALISP_COMPILER_NAME;
constexpr static const bool debug_build = ALISP_DEBUG;

struct BuildInfo
{

    [[nodiscard]] constexpr static int version_major() noexcept
    {
        return alisp::version_major;
    }

    [[nodiscard]] constexpr static int version_minor() noexcept
    {
        return alisp::version_minor;
    }

    [[nodiscard]] constexpr static int version_patch() noexcept
    {
        return alisp::version_patch;
    }

    [[nodiscard]] constexpr static bool debug_build() noexcept
    {
        return alisp::debug_build;
    }

    [[nodiscard]] static std::string version()
    {
        return std::to_string(version_major()) + '.' + std::to_string(version_minor()) + '.' + std::to_string(version_patch());
    }
};

const std::string get_build_info() noexcept;

enum class Options
{
    LOAD_PRELUDE
};

inline std::vector<Options> default_options()
{
    return {Options::LOAD_PRELUDE};
}


}
