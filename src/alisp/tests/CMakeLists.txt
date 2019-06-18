




get_target_property(TEST_SOURCES "${PROJECT_NAME}_alisp" SOURCES)
add_executable(${PROJECT_NAME}_alisp_test
    test_lexer.cpp
    ../src/common_lexer.cpp
    ../src/lexer.cpp
    ../src/parser.cpp
    ../src/lisp_object.cpp
    ../src/error_messaging.cpp
    )

target_include_directories(${PROJECT_NAME}_alisp_test
    PUBLIC ../include
    PRIVATE ../src
    )





target_link_libraries(${PROJECT_NAME}_alisp_test
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    rang::rang
    alisp_common    
    Threads::Threads
    fmt::fmt
    spdlog::spdlog
    absl::strings
    Catch2::Catch2)


include(CTest)
include(Catch)
catch_discover_tests(${PROJECT_NAME}_alisp_test)
