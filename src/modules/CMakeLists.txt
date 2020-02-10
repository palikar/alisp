macro(add_dynmodule TARGET_NAME NAME SRC)    
    add_library(${TARGET_NAME} MODULE ${SRC})

    target_include_directories(${TARGET_NAME}
        PUBLIC include
        PRIVATE src)

    target_link_libraries(${TARGET_NAME}
        PRIVATE
        project_options
        project_warnings
        alisp_language
        fmt::fmt)
    
    set_target_properties(${TARGET_NAME} PROPERTIES PREFIX "")
    set_target_properties(${TARGET_NAME} PROPERTIES OUTPUT_NAME "${NAME}")
    set_target_properties(${TARGET_NAME} PROPERTIES SUFFIX ".so")
    
    install(TARGETS ${TARGET_NAME} DESTINATION ${IMPORT_PATHS})    
endmacro()



add_dynmodule(alisp_module_base64 base64 src/base64.cpp)
add_dynmodule(alisp_module_fmt fmt src/fmt.cpp)
add_dynmodule(alisp_module_func func src/func.cpp)
add_dynmodule(alisp_module_http http src/http.cpp)
add_dynmodule(alisp_module_json json src/json.cpp)
add_dynmodule(alisp_module_locale locale src/locale.cpp)
add_dynmodule(alisp_module_process process src/process.cpp)
add_dynmodule(alisp_module_random random src/random.cpp)
add_dynmodule(alisp_module_re re src/re.cpp)
add_dynmodule(alisp_module_xml xml src/xml.cpp)


