add_library(alisp_util
    ./src/common.cpp
    ./src/logging.cpp
    ./src/defines.cpp
    )


target_include_directories(alisp_util
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(alisp_util
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    Threads::Threads
    fmt::fmt
    spdlog::spdlog
    )


install(TARGETS ${PROJECT_NAME}_util
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
	)


if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
