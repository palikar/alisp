add_executable(alisp_language_test
    test_main.cpp
    test_parser.cpp
    test_env.cpp
    test_common.cpp
    test_eval.cpp
    test_engine.cpp
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
    alisp_language
    )


target_link_libraries(alisp_language_test PRIVATE alisp_fileio
    alisp_math alisp_time alisp_system alisp_platform)

include(CTest)
include(Catch)

catch_discover_tests(alisp_language_test)
