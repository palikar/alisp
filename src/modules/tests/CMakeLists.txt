add_executable(${alisp}_modules_test
	main_test.cpp)

target_include_directories(${alisp}_modules_test
	PUBLIC ../include
	PRIVATE ../src
	)

target_link_libraries(${alisp}_modules_test
    PRIVATE
	project_options
    project_warnings
	PUBLIC
	Catch2::Catch2)


include(CTest)
include(Catch)
catch_discover_tests(${alisp}_modules_test)
