macro(add_dynmodule TARGET_NAME NAME SRC)    
    add_library(${TARGET_NAME} MODULE ${SRC})

    target_include_directories(${TARGET_NAME}
        PUBLIC include
        PRIVATE src)

    target_link_libraries(${TARGET_NAME}
        PRIVATE
        project_options
        project_warnings
        language_options
        alisp_language
        fmt::fmt)
    
    set_target_properties(${TARGET_NAME} PROPERTIES PREFIX "")
    set_target_properties(${TARGET_NAME} PROPERTIES OUTPUT_NAME "${NAME}")
    set_target_properties(${TARGET_NAME} PROPERTIES SUFFIX ".so")
    
    install(TARGETS ${TARGET_NAME} DESTINATION ${IMPORT_PATHS})
endmacro()

# set(CMAKE_LINK_WHAT_YOU_USE OFF)

add_dynmodule(alisp_module_base64 base64 src/base64.cpp) 
add_dynmodule(alisp_module_fmt fmt src/fmt.cpp)
add_dynmodule(alisp_module_func func src/func.cpp)
add_dynmodule(alisp_module_json json src/json.cpp)
add_dynmodule(alisp_module_locale locale src/locale.cpp)
add_dynmodule(alisp_module_process process src/process.cpp)
add_dynmodule(alisp_module_random random src/random.cpp)
add_dynmodule(alisp_module_re re src/re.cpp)
add_dynmodule(alisp_module_xml xml src/xml.cpp)
add_dynmodule(alisp_module_request request src/request.cpp)
add_dynmodule(alisp_module_asserts asserts src/asserts.cpp)
add_dynmodule(alisp_module_http http src/http.cpp)


target_link_libraries(alisp_module_request PUBLIC cpr)
target_link_libraries(alisp_module_process PUBLIC alisp_management)
target_link_libraries(alisp_module_xml PUBLIC tinyxml2)

target_link_libraries(alisp_module_http PUBLIC usockets uwebsockets alisp_management)

set_target_properties(usockets PROPERTIES LINK_WHAT_YOU_USE OFF)
set_target_properties(alisp_module_http PROPERTIES LINK_WHAT_YOU_USE OFF)




if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
