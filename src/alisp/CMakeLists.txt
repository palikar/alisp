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



install(TARGETS alisp_language
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )



if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
