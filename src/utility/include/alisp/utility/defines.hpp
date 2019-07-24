#pragma once

#include "alisp/config.hpp"
#include <fmt/format.h>

#define ALISP_COMPILER_VERSION __VERSION__

#if defined(__GNUC__)
#define ALISP_COMPILER_NAME "gcc"
#endif


#ifdef _DEBUG
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


[[nodiscard]] static inline const std::string get_build_info() noexcept
{
    std::string build_str = fmt::format("ALisp {}.{}.{}\n",
                                        version_major, version_minor, version_patch);
    build_str += fmt::format("[{} {}] ", compiler_name, compiler_version);
    build_str += "Build:";
    build_str += debug_build ? " debug":" Release";
    build_str += "\n";
    return build_str;
}

enum class Options
{
    LOAD_PRELUDE
};

static inline std::vector<Options> default_options()
{
    return {Options::LOAD_PRELUDE};
}


}
