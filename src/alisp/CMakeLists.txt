macro(add_module NAME SRC)
    add_library(${NAME} ${SRC})

    target_include_directories(${NAME}
        PUBLIC include
        PRIVATE src)

    if(NOT BUILD_SHARED_LIBS)

        
        target_link_libraries(${NAME}
            PRIVATE
            project_options
            project_warnings
            language_options
            alisp_util
            alisp_management
            alisp_streams
            alisp_language
            )
    else()
        
        target_link_libraries(${NAME}
            PRIVATE
            language_options
            project_options
            project_warnings
            alisp_util
            alisp_management
            alisp_streams)
        
    endif()
    

    install(TARGETS ${NAME} 
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
        RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
        INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
        ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
        )

endmacro()

add_library(language_options INTERFACE)

set(PRELUDE_DIR ${CMAKE_INSTALL_FULL_DATAROOTDIR}/${PROJECT_NAME}/prelude)

target_compile_definitions(language_options INTERFACE AL_PRELUDE_DIR=\"${PRELUDE_DIR}\")

if(ENABLE_STACK_TRACE)
    target_compile_definitions(language_options INTERFACE ENABLE_STACK_TRACE)
endif(ENABLE_STACK_TRACE)

if(ENABLE_LINE_TRACE)
    target_compile_definitions(language_options INTERFACE ENABLE_LINE_TRACE)
endif(ENABLE_LINE_TRACE)

if(ENABLE_OBJECT_DOC)
    target_compile_definitions(language_options INTERFACE ENABLE_OBJECT_DOC)
endif(ENABLE_OBJECT_DOC)

if(DISABLE_RUN_TIME_CHECKS)
    target_compile_definitions(language_options INTERFACE DISABLE_CHECKS)
endif(DISABLE_RUN_TIME_CHECKS)


add_library(alisp_language
    
    src/alisp_parser.cpp
    src/alisp_env.cpp
    src/alisp_eval.cpp
    src/alisp_object.cpp
    src/alisp_engine.cpp
    src/alisp_modules.cpp
    src/alisp_streams.cpp
    src/alisp_files.cpp
    src/alisp_memory.cpp
    src/alisp_warnings.cpp
    src/alisp_loadable_modules.cpp

    src/alisp_eval_functions.cpp
    src/alisp_stream_functions.cpp
    src/alisp_file_functions.cpp
    src/alisp_language.cpp
    src/alisp_lists.cpp
    src/alisp_logic.cpp
    src/alisp_math.cpp
    src/alisp_bits.cpp
    src/alisp_predicates.cpp
    src/alisp_printing.cpp
    src/alisp_algorithms.cpp
    src/alisp_strings.cpp
    src/alisp_casts.cpp
    src/alisp_props.cpp)


target_include_directories(alisp_language
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(alisp_language
    PRIVATE
    project_options
    project_warnings
    language_options
    dl
    PUBLIC
    ${CMAKE_DL_LIBS}
    alisp_util
    alisp_streams
    alisp_management
    fmt::fmt
    )


if(NOT BUILD_SHARED_LIBS)
    set_property(TARGET alisp_language PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

if(NOT DISABLE_DEFAULT_MODULES)

    add_module(alisp_fileio src/modules/fileio.cpp)
    add_module(alisp_math src/modules/math.cpp)
    add_module(alisp_time src/modules/time.cpp)
    add_module(alisp_system src/modules/system.cpp)
    add_module(alisp_platform src/modules/platform.cpp)
    add_module(alisp_memory src/modules/memory.cpp)

    target_link_libraries(alisp_language PUBLIC alisp_fileio)
    target_link_libraries(alisp_language PUBLIC alisp_math)
    target_link_libraries(alisp_language PUBLIC alisp_time)
    target_link_libraries(alisp_language PUBLIC alisp_system)
    target_link_libraries(alisp_language PUBLIC alisp_platform)
    target_link_libraries(alisp_language PUBLIC alisp_memory)

    target_compile_definitions(alisp_language PUBLIC LINK_MODULES)
    
endif()

install(TARGETS alisp_language 
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )

install(DIRECTORY ./data/prelude/ DESTINATION ${PRELUDE_DIR})
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/data/libs/" DESTINATION ${IMPORT_PATHS})


if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
