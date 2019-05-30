add_executable(numler_alisp_test
	main_test.cpp)

target_include_directories(numler_alisp_test
	PUBLIC ../include
	PRIVATE ../src
	)

target_link_libraries(numler_alisp_test
    PRIVATE
	project_options
    project_warnings
	PUBLIC
	Catch2::Catch2)


include(CTest)
include(Catch)
catch_discover_tests(numler_alisp_test)
