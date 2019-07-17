add_executable(alisp
    src/main.cpp
    src/prompt.cpp
    )


target_include_directories(alisp
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(alisp
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    Threads::Threads
    fmt::fmt
    spdlog::spdlog
    alisp_language
    readline
    alisp_util
    clara::clara
    )



install(TARGETS alisp
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${alisp}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${alisp}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )





if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()