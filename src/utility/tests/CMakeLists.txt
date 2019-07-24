add_executable(${PROJECT_NAME}_common_test
    main_test.cpp)

target_include_directories(${PROJECT_NAME}_common_test
    PUBLIC ../include
    PRIVATE ../src
    )

    target_link_libraries(${PROJECT_NAME}_common_test
        PRIVATE
	project_options
    project_warnings
	PUBLIC
	Catch2::Catch2)


include(CTest)
include(Catch)
catch_discover_tests(${PROJECT_NAME}_common_test)
