
add_executable(${PROJECT_NAME}_alisp
    src/main.cpp
    src/common_lexer.cpp
    src/lexer.cpp
    src/main.cpp    
    src/parser.cpp
    src/lisp_object.cpp
    src/error_messaging.cpp
    src/prompt.cpp
    )


target_include_directories(${PROJECT_NAME}_alisp
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(${PROJECT_NAME}_alisp
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    clara::clara
    rang::rang
    alisp_common    
    Threads::Threads
    fmt::fmt
    spdlog::spdlog
    absl::strings
    readline
    )



install(TARGETS ${PROJECT_NAME}_alisp
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )





if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
