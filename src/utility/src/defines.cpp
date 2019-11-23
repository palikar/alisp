
#include "alisp/utility/defines.hpp"




namespace alisp
{

const std::string get_build_info() noexcept
{
    std::string build_str = fmt::format("ALisp {}.{}.{}\n",
                                        version_major, version_minor, version_patch);
    build_str += fmt::format("[{} {}] ", compiler_name, compiler_version);
    build_str += "Build:";
    build_str += debug_build ? " Debug":" Release";
    build_str += "\n";
    return build_str;

}

}
