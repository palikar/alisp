add_executable(alisp_applications_test
    main_tests.cpp)

target_include_directories(alisp_applications_test
    PUBLIC ../include
    PRIVATE ../src
    )

target_link_libraries(alisp_applications_test
    PRIVATE
    project_options
    project_warnings
    PUBLIC)


include(CTest)
include(Catch)
catch_discover_tests(alisp_applications_test)
