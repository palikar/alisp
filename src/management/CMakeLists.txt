add_library(alisp_management
    ./src/main.cpp
    )


target_include_directories(alisp_management
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(alisp_management
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    fmt::fmt)


if(NOT BUILD_SHARED_LIBS)
    set_property(TARGET alisp_management PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

install(TARGETS alisp_management
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${alisp}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${alisp}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )

install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})



if(ENABLE_TESTING)
	add_subdirectory(tests)
endif()
