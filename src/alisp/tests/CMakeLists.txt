get_target_property(LAN_SOURCES alisp_language SOURCES)
list(TRANSFORM LAN_SOURCES PREPEND "../")

add_executable(alisp_language_test
    test_main.cpp
    test_parser.cpp
    test_env.cpp
    test_common.cpp
    test_eval.cpp
    test_props.cpp
    test_algs.cpp
    test_cast.cpp
    test_engine.cpp
    test_files.cpp
    test_memory.cpp
    ${LAN_SOURCES})

target_include_directories(alisp_language_test
    PUBLIC ../include
    PRIVATE ../src
    )


set(EXAMPLE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/../data/tests/simple_file.al")
set(TEXT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/../data/tests/example_text_file.txt")
set(OUTPUT_FILE "${CMAKE_CURRENT_SOURCE_DIR}/../data/tests/output_text_file.txt")

target_compile_definitions(alisp_language_test PUBLIC AL_PRELUDE_DIR=\"${PRELUDE_DIR}\")
target_compile_definitions(alisp_language_test PUBLIC EXAMPLE_FILE=\"${EXAMPLE_FILE}\")
target_compile_definitions(alisp_language_test PUBLIC TEXT_FILE=\"${TEXT_FILE}\")
target_compile_definitions(alisp_language_test PUBLIC OUTPUT_FILE=\"${OUTPUT_FILE}\")

target_link_libraries(alisp_language_test
    PRIVATE
    project_options
    project_warnings
    language_options
    dl
    PUBLIC
    alisp_language
    )

# target_link_libraries(alisp_language_test PRIVATE alisp_fileio
#     alisp_math alisp_time alisp_system alisp_platform)

include(CTest)
include(Catch)

catch_discover_tests(alisp_language_test)

if(ENABLE_ASAN)
    
    add_executable(alisp_fuzzy_test
        fuzzy_test.cpp
        ${LAN_SOURCES})
    
    target_include_directories(alisp_fuzzy_test
        PUBLIC ../include
        PRIVATE ../src
        )
    target_compile_options(alisp_fuzzy_test
        PUBLIC -fsanitize=address,fuzzer)

    target_link_libraries(alisp_fuzzy_test
        PRIVATE
        project_options
        project_warnings
        -fsanitize=address,fuzzer
        dl
        PUBLIC
        alisp_language)
    
endif()
