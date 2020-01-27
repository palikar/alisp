add_library(test_alisp_module MODULE
    src/main.cpp
    )


target_include_directories(test_alisp_module
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(test_alisp_module
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    fmt::fmt
    alisp_language
    )

set_target_properties(test_alisp_module PROPERTIES PREFIX "")
set_target_properties(test_alisp_module PROPERTIES OUTPUT_NAME "test")
set_target_properties(test_alisp_module PROPERTIES SUFFIX ".so")


install(
    TARGETS test_alisp_module
    DESTINATION ${IMPORT_PATHS})




if(ENABLE_TESTING)
	add_subdirectory(tests)
endif()
