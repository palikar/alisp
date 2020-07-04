add_library(alisp_util
    ./src/common.cpp
    ./src/logging.cpp
    ./src/defines.cpp
    ./src/files.cpp
    ./src/env.cpp
    )

if(MSV)
    target_sources(alisp_util PRIVATE ./src/windows/system.cpp)
else()
    target_sources(alisp_util PRIVATE ./src/linux/system.cpp)
endif(MSV)


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
    )

if(NOT BUILD_SHARED_LIBS)
    set_property(TARGET alisp_util PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

install(TARGETS ${PROJECT_NAME}_util
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
	)
    install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

    if(ENABLE_TESTING)
    add_subdirectory(tests)
endif()
