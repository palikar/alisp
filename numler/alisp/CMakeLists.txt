add_executable(numler_alisp
    src/main.cpp
    )


target_include_directories(numler_alisp
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(numler_alisp
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    numler_common
    Threads::Threads
    fmt::fmt
    spdlog::spdlog
    )



install(TARGETS numler_main_app
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/numler/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/numler/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )





if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
