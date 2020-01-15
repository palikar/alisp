# add_library(alisp_fileio
#     src/modules/fileio.cpp)

# target_include_directories(alisp_fileio
#     PUBLIC include
#     PRIVATE src)


# target_link_libraries(alisp_fileio
#     PRIVATE
#     project_options
#     project_warnings
#     alisp_util)

macro(add_module NAME SRC)    
    add_library(${NAME} ${SRC})

    target_include_directories(${NAME}
        PUBLIC include
        PRIVATE src)


    target_link_libraries(${NAME}
        PRIVATE
        project_options
        project_warnings
        alisp_util)
endmacro()


add_library(alisp_language
    src/alisp_parser.cpp
    src/alisp_env.cpp
    src/alisp_eval.cpp
    src/alisp_object.cpp

    src/alisp_language.cpp
    src/alisp_lists.cpp
    src/alisp_logic.cpp
    src/alisp_math.cpp
    src/alisp_predicates.cpp
    src/alisp_printing.cpp
    src/alisp_engine.cpp
    src/alisp_modules.cpp
    )


target_include_directories(alisp_language
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(alisp_language
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    alisp_util
    fmt::fmt
    )


add_module(alisp_fileio src/modules/fileio.cpp)
target_link_libraries(alisp_language PRIVATE alisp_fileio)

add_module(alisp_math src/modules/math.cpp)
target_link_libraries(alisp_language PRIVATE alisp_math)

add_module(alisp_time src/modules/time.cpp)
target_link_libraries(alisp_language PRIVATE alisp_time)

add_module(alisp_system src/modules/system.cpp)
target_link_libraries(alisp_language PRIVATE alisp_system)



install(TARGETS alisp_language alisp_fileio
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )



if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
