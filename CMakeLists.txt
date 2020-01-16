cmake_minimum_required(VERSION 3.10)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

project(alisp LANGUAGES CXX)

include(cmake/safeguards.cmake)

include(GNUInstallDirs)
include(FindUnixCommands)


#######################################################################
#######################################################################
############################     Options     ##########################
#######################################################################
#######################################################################


option(ONLY_COVERAGE "Build only tests necessary for coverage" FALSE)
option(LIBCPP "Build with libc++" FALSE)
option(ENABLE_COVERAGE "Enable coverage reporting for gcc/clang" FALSE)
option(ENABLE_ASAN "Enable address sanitizer" FALSE)
option(BUILD_SHARED_LIBS "Enable compilation of shared libraries" FALSE)
option(ENABLE_TESTING "Enable the building of the test" FALSE)
option(ENABLE_CLANG_TIDY "Enable testing with clang-tidy" FALSE)
option(ENABLE_CPPCHECK "Enable testing with cppcheck" FALSE)
option(SIMPLE_BUILD "Build the project as minimally as possible" FALSE)
option(BUILD_DOC "Build the project's documentation" FALSE)
option(FORCE_COLORED_OUTPUT "Always produce ANSI-colored output (GNU/Clang only)." TRUE)
option(ELF_HASHING "Use elf as a hashing function" FALSE)
option(JENKINS_HASHING "Use jenkins one at a time as a hashing function" FALSE)
option(DEBUG_LOGGING "Enabling debug logging" FALSE)
option(BUILD_EXAMPLES "Enable targets for running the example scripts" FALSE)
option(MANUAL_MEMORY "Use raw pointers instead of smart ones" FALSE)
option(VALGRIND_CHECKS "Enable Valgrind tools to perform checks on the application " FALSE)
option(RUN_PERFORMANCE_TESTS "Build support for the performance tests" FALSE)


set(TIME_CHECK_SAMPLES 10 CACHE STRING "How many time to perform the timing check") 


#######################################################################
#######################################################################



message("+++++++${PROJECT_NAME}++++++++++")

set(CMAKE_CXX_STANDARD 17)

add_library(project_warnings INTERFACE)
add_library(project_options INTERFACE)


target_compile_features(project_options INTERFACE cxx_std_17)


if(ONLY_COVERAGE OR ENABLE_COVERAGE)
    target_compile_options(project_options INTERFACE --coverage -O0 -g)
    target_link_libraries(project_options INTERFACE --coverage)
endif()


if(ENABLE_ASAN)
    target_compile_options(project_options INTERFACE -fsanitize=address)
    target_link_libraries(project_options INTERFACE -fsanitize=address)
endif()



target_compile_options(project_warnings
    INTERFACE
    -Wall
    # -Werror
    -Wextra
    -Wshadow
    -Wnon-virtual-dtor
    -Wold-style-cast
    -Wcast-align
    # -Wunused
    -Woverloaded-virtual
    -Wpedantic
    -Wconversion
    -Wsign-conversion
    -Wnull-dereference
    -Wdouble-promotion
    -Wformat=2
    )


if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    target_compile_options(project_warnings
        INTERFACE
        -Wmisleading-indentation
        # -Wduplicated-cond
        # -Wduplicated-branches
        -Wlogical-op
        -Wuseless-cast
        )
    target_link_libraries(project_options INTERFACE stdc++fs)
endif()

if (${FORCE_COLORED_OUTPUT})
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
        target_compile_options (project_options INTERFACE -fdiagnostics-color=always)
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
        target_compile_options (project_options INTERFACE -fcolor-diagnostics)
    endif ()
endif ()



find_program(CCACHE ccache)
if(CCACHE)
    set(CMAKE_CXX_COMPILER_LAUNCHER ${CCACHE})
endif()



if(ENABLE_CPPCHECK)
    find_program(CPPCHECK cppcheck)
    if(CPPCHECK)
	set(CMAKE_CXX_CPPCHECK
	    ${CPPCHECK}
	    --suppress=syntaxError
	    --enable=all
	    --inconclusive)
    else()
	message(SEND_ERROR "cppcheck requested but executable not found")
    endif()
endif()

if(ENABLE_CLANG_TIDY)
    find_program(CLANGTIDY clang-tidy)
    if(CLANGTIDY)
	set(CMAKE_CXX_CLANG_TIDY ${CLANGTIDY})
    else()
	message(SEND_ERROR "clang-tidy requested but executable not found")
    endif()
endif()


if(NOT EXISTS "${CMAKE_BINARY_DIR}/conan.cmake")
    message(STATUS "Downloading conan.cmake from https://github.com/conan-io/cmake-conan")
    file(DOWNLOAD  "https://raw.githubusercontent.com/conan-io/cmake-conan/master/conan.cmake"
        "${CMAKE_BINARY_DIR}/conan.cmake")
endif()

include(${CMAKE_BINARY_DIR}/conan.cmake)

conan_check(REQUIRED)
conan_cmake_run(
    CONANFILE
    conanfile.txt
    BASIC_SETUP
    CONAN_COMMAND
    ${CONAN_CMD}
    CMAKE_TARGETS
    BUILD
    missing)
include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR})

if(ENABLE_TESTING)
    include(Catch)
    enable_testing()
    
    if(ONLY_COVERAGE OR ENABLE_COVERAGE)

        include(CodeCoverage)

        set(COVERAGE_GCOVR_EXCLUDES "${PROJECT_SOURCE_DIR}/libs/include/*" "${PROJECT_SOURCE_DIR}/libs/include/catch2/*")

        setup_target_for_coverage_gcovr_html(
            NAME alisp_coverage_html
            EXECUTABLE ./bin/alisp_language_test
            DEPENDENCIES alisp_language_test)
        
        setup_target_for_coverage_gcovr_xml(
            NAME alisp_coverage_xml
            EXECUTABLE ./bin/alisp_language_test
            DEPENDENCIES alisp_language_test)
    endif()
    
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS 1)

#######################################################################
#######################################################################
###########################    Libraries     ##########################
#######################################################################
#######################################################################


if(NOT MINGW)
    find_library(READLINE_LIBRARY NAMES readline PATH /usr/lib /usr/local/lib /opt/local/lib)
endif()


message(STATUS "Detecting readline support")
if(READLINE_LIBRARY)
    message(STATUS "Found: ${READLINE_LIBRARY}")
    set(READLINE_LIB readline)
    add_definitions(/DREADLINE_AVAILABLE)
else()
    message(STATUS "Not Found")
    set(READLINE_LIB)
    set(READLINE_FLAG)
endif()



set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

find_package(fmt)

#######################################################################
#######################################################################



set(PROJECT_BRIEF "The alisp scripting language")


if(ELF_HASHING)
    target_compile_definitions(project_options INTERFACE -DELF_HASHING)
endif(ELF_HASHING)


if(JENKINS_HASHING)
    target_compile_definitions(project_options INTERFACE -DJENKINS_HASHING)
endif(JENKINS_HASHING)

if(MANUAL_MEMORY)
    target_compile_definitions(project_options INTERFACE -DUSE_MANUAL_MEMORY)
endif(MANUAL_MEMORY)

if(DEBUG_LOGGING)
    target_compile_definitions(project_options INTERFACE -DDEUBG_LOGGING)
endif(DEBUG_LOGGING)

file(READ "${PROJECT_SOURCE_DIR}/version" VERSION NEWLINE_CONSUME)
string(REPLACE "\n" "" VERSION "${VERSION}")
string(REPLACE "." ";" VERSION_LIST ${VERSION})
list(GET VERSION_LIST 0 AL_VERSION_MAJOR)
list(GET VERSION_LIST 1 AL_VERSION_MINOR)
list(GET VERSION_LIST 2 AL_VERSION_PATCH)
set(AL_VERSION ${AL_VERSION_MAJOR}.${AL_VERSION_MINOR}.${AL_VERSION_PATCH})

set(IMPORT_PATHS ${CMAKE_INSTALL_FULL_DATAROOTDIR}/${PROJECT_NAME})

configure_file (
    "${PROJECT_SOURCE_DIR}/src/include/${PROJECT_NAME}/config.hpp.in"
    "${PROJECT_BINARY_DIR}/src/include/${PROJECT_NAME}/config.hpp"
    )

include_directories("${PROJECT_BINARY_DIR}/src/include")
include_directories("${PROJECT_SOURCE_DIR}/src/include")
include_directories("${PROJECT_SOURCE_DIR}/libs/include")

install (FILES
    "${PROJECT_BINARY_DIR}/src/include/${PROJECT_NAME}/config.hpp"
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    )


include (InstallRequiredSystemLibraries)
set(CPACK_PACKAGE_VENDOR "Stanislav Arnaudov")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "The alisp interpreted langege.")
set(CPACK_PACKAGE_VERSION_MAJOR ${AL_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${AL_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${AL_VERSION_PATCH})
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_SOURCE_GENERATOR "TGZ;ZIP")
set(CPACK_GENERATOR "DEB")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Stanislav Arnaudov")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/palikar/alisp")
set(CPACK_SOURCE_IGNORE_FILES
    /.git
    /.github
    /dist
    /.*build.*
    /\\\\.DS_Store)
include(CPack)



if(BUILD_DOC)
    find_package(Doxygen)
    if (DOXYGEN_FOUND)
	# set input and output files
	set(DOXYGEN_IN ${CMAKE_CURRENT_SOURCE_DIR}/doc/Doxyfile.in)
	set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

	# request to configure the file
	configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)
	message(STATUS "Doxygen build started")

	# note the option ALL which allows to build the docs together with the application
	add_custom_target(doc ALL
	    COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
	    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
	    COMMENT "Generating API documentation with Doxygen"
	    VERBATIM )
    else (DOXYGEN_FOUND)
	message("Doxygen need to be installed to generate the doxygen documentation")
    endif (DOXYGEN_FOUND)
endif()


add_subdirectory(libs)
add_subdirectory(src)

install(
    FILES ${CMAKE_CURRENT_SOURCE_DIR}/scripts/libs/format.al
    DESTINATION ${IMPORT_PATHS})



if(BUILD_EXAMPLES)

    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/scripts DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
    add_custom_command(
        OUTPUT SYMBOLIC ${CMAKE_CURRENT_BINARY_DIR}/scripts_results
        COMMAND ALISP_EXE=../bin/alisp make
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/scripts
        USES_TERMINAL
        )
    
    add_custom_target(test_examples DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/scripts_results alisp)

endif(BUILD_EXAMPLES)

if(VALGRIND_CHECKS)
    
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/valgrind_massif.out
        COMMAND valgrind --tool=massif --log-file=valgrind_massif.out ./bin/alisp_language_test
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        USES_TERMINAL)


    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/valgrind_leaks.out
        COMMAND valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --log-file=valgrind_leaks.out ./bin/alisp_language_test 
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        USES_TERMINAL)


    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/valgrind_memcheck.out
        COMMAND valgrind  --tool=memcheck --log-file=valgrind_memcheck.out ./bin/alisp_language_test 
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        USES_TERMINAL)


    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/valgrind_callgrind.out
        COMMAND valgrind  --tool=callgrind  --log-file=valgrind_callgrind.out ./bin/alisp_language_test 
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        USES_TERMINAL)


    add_custom_target(valgrind_checks DEPENDS alisp alisp_language_test
        ${CMAKE_CURRENT_BINARY_DIR}/valgrind_leaks.out
        ${CMAKE_CURRENT_BINARY_DIR}/valgrind_callgrind.out
        ${CMAKE_CURRENT_BINARY_DIR}/valgrind_memcheck.out
        ${CMAKE_CURRENT_BINARY_DIR}/valgrind_massif.out)

endif(VALGRIND_CHECKS)



if (RUN_PERFORMANCE_TESTS)

    file(GLOB PERFORMANCE_TESTS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/performance_tests/ ${CMAKE_CURRENT_SOURCE_DIR}/performance_tests/*.al)
    list(SORT PERFORMANCE_TESTS)

    set(PERFORMANCE_TESTS_FILES "")

    foreach(filename ${PERFORMANCE_TESTS})
        message(STATUS "Adding performance test ${filename}")

        add_test(NAME performance.${filename}
            COMMAND ${CMAKE_CURRENT_BINARY_DIR}/bin/alisp ${CMAKE_CURRENT_SOURCE_DIR}/performance_tests/${filename} > /dev/null
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            )
        
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/performance.${filename}.out
            COMMAND time -o ${CMAKE_CURRENT_BINARY_DIR}/performance.${filename}.out ${CMAKE_CURRENT_BINARY_DIR}/bin/alisp ${CMAKE_CURRENT_SOURCE_DIR}/performance_tests/${filename} > /dev/null
            VERBATIM
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            )
        
        list(APPEND PERFORMANCE_TESTS_FILES ${CMAKE_CURRENT_BINARY_DIR}/performance.${filename}.out)
        list(APPEND TESTS performance.${filename})        
    endforeach()    

    
    add_custom_target(performance_check DEPENDS alisp alisp_language_test
        ${PERFORMANCE_TESTS_FILES})


    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/timing_checks.txt
        COMMAND seq ${TIME_CHECK_SAMPLES} | xargs -I -- ctest -R "performance\\." | grep -i "total test time" | grep "\\d+\\.\\d+" -P -o | xargs -I {} -- echo -n {}, | tee -a ${CMAKE_CURRENT_BINARY_DIR}/timing_checks.txt > /dev/null
        VERBATIM
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

    add_custom_target(timing_check DEPENDS alisp ${CMAKE_CURRENT_BINARY_DIR}/timing_checks.txt)

    
endif()



message("#########################################")
message("\t \t Summary")
message("#########################################")
message("Build type:       \t ${CMAKE_BUILD_TYPE}")
message("Install prefix:   \t ${CMAKE_INSTALL_PREFIX}")
message("Testing enabled:  \t ${ENABLE_TESTING}")
message("Clang-tidy:       \t ${ENABLE_CLANG_TIDY}")
message("Cppcheck:         \t ${ENABLE_CPPCHECK}")
message("Compiler:         \t ${CMAKE_CXX_COMPILER_ID}")
message("Sanizizers:       \t ${ENABLE_ASAN}")
message("Shared libs:      \t ${BUILD_SHARED_LIBS}")
message("Build libcpp:     \t ${LIBCPP}")
message("CCache executable:\t ${CCACHE}")
message("Building doc:     \t ${BUILD_DOC}")
message("Valgrind Checks:  \t ${VALGRIND_CHECKS}")
message("Simple build:     \t ${SIMPLE_BUILD}")
message("Jenkins Hashing:  \t ${JENKINS_HASHING}")
message("Elf Hashing:      \t ${ELF_HASHING}")
message("Debug Logging:    \t ${DEBUG_LOGGING}")
message("Build exampls:    \t ${BUILD_EXAMPLES}")
message("Manual Memory:    \t ${MANUAL_MEMORY}")
message("Performance tests:\t ${RUN_PERFORMANCE_TESTS}")
message("------------------------------------------")
message("Version:          \t ${AL_VERSION}")
message("########################################")
