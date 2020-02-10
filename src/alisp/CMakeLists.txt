macro(add_module NAME SRC)    
    add_library(${NAME} ${SRC})

    target_include_directories(${NAME}
        PUBLIC include
        PRIVATE src)

    target_link_libraries(${NAME}
        PRIVATE
        project_options
        project_warnings
        alisp_util
        alisp_management
        alisp_streams
        )
endmacro()


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

    src/alisp_stream_functions.cpp
    src/alisp_file_functions.cpp
    src/alisp_language.cpp
    src/alisp_lists.cpp
    src/alisp_logic.cpp
    src/alisp_math.cpp
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
    dl
    PUBLIC
    ${CMAKE_DL_LIBS}
    alisp_util
    alisp_streams
    alisp_management
    fmt::fmt
    )


add_module(alisp_fileio src/modules/fileio.cpp)
target_link_libraries(alisp_language PUBLIC alisp_fileio)

add_module(alisp_math src/modules/math.cpp)
target_link_libraries(alisp_language PUBLIC alisp_math)

add_module(alisp_time src/modules/time.cpp)
target_link_libraries(alisp_language PUBLIC alisp_time)

add_module(alisp_system src/modules/system.cpp)
target_link_libraries(alisp_language PUBLIC alisp_system)

add_module(alisp_platform src/modules/platform.cpp)
target_link_libraries(alisp_language PUBLIC alisp_platform)

add_module(alisp_memory src/modules/memory.cpp)
target_link_libraries(alisp_language PUBLIC alisp_memory)

set(PRELUDE_DIR ${CMAKE_INSTALL_FULL_DATAROOTDIR}/${PROJECT_NAME}/prelude)
    
target_compile_definitions(alisp_language PUBLIC LINK_MODULES)
target_compile_definitions(alisp_language PUBLIC AL_PRELUDE_DIR=\"${PRELUDE_DIR}\")



install(TARGETS alisp_language alisp_fileio alisp_math
    alisp_time alisp_system alisp_platform alisp_memory
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )

install(DIRECTORY ./data/prelude/ DESTINATION ${PRELUDE_DIR})



if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
