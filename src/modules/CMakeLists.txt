macro(add_dynmodule NAME SRC)    
    add_library(${NAME} MODULE ${SRC})

    target_include_directories(${NAME}
        PUBLIC include
        PRIVATE src)

    target_link_libraries(${NAME}
        PRIVATE
        project_options
        project_warnings
        alisp_language
        fmt::fmt)
    
    set_target_properties(${NAME} PROPERTIES PREFIX "")
    set_target_properties(${NAME} PROPERTIES OUTPUT_NAME "${NAME}")
    set_target_properties(${NAME} PROPERTIES SUFFIX ".so")
    
    install(TARGETS ${NAME} DESTINATION ${IMPORT_PATHS})
    
endmacro()



add_dynmodule(xml src/xml.cpp)


