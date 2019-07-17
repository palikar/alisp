add_executable(${alisp}_applications_test
    main_test.cpp)

target_include_directories(${alisp}_applications_test
    PUBLIC ../include
    PRIVATE ../src
    )

target_link_libraries(${alisp}_applications_test
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    Catch2::Catch2)


include(CTest)
include(Catch)
catch_discover_tests(${alisp}_applications_test)
