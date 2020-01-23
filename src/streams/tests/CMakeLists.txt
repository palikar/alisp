add_executable(alisp_streams_test
    main_test.cpp)

target_include_directories(alisp_streams_test
    PUBLIC ../include
    PRIVATE ../src
    )

target_link_libraries(alisp_streams_test
    PRIVATE
    project_options
    project_warnings)


include(CTest)
include(Catch)
catch_discover_tests(alisp_streams_test)
