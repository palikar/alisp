add_executable(alisp_language_test
    test_main.cpp
    test_parser.cpp
    test_env.cpp
    test_common.cpp

    ../src/alisp_parser.cpp
    ../src/alisp_env.cpp
    ../src/alisp_eval.cpp
    ../src/alisp_object.cpp

    ../src/alisp_language.cpp
    ../src/alisp_lists.cpp
    ../src/alisp_logic.cpp
    ../src/alisp_math.cpp
    ../src/alisp_predicates.cpp
    ../src/alisp_printing.cpp

    )

target_include_directories(alisp_language_test
    PUBLIC ../include
    PRIVATE ../src
    )





target_link_libraries(alisp_language_test
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    alisp_util
    rang::rang
    Threads::Threads
    fmt::fmt
    spdlog::spdlog
    Catch2::Catch2)



include(CTest)
include(Catch)

catch_discover_tests(alisp_language_test)
