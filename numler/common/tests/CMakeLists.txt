add_executable(numler_common_test
	main_test.cpp)

target_include_directories(numler_common_test
	PUBLIC ../include
	PRIVATE ../src
	)

target_link_libraries(numler_common_test
    PRIVATE
	project_options
    project_warnings
	PUBLIC
	Catch2::Catch2)


include(CTest)
include(Catch)
catch_discover_tests(numler_common_test)
