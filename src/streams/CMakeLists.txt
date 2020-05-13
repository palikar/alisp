add_library(alisp_streams
    ./src/streams.cpp
    )


target_include_directories(alisp_streams
    PUBLIC include
    PRIVATE src
    )

target_link_libraries(alisp_streams
    PRIVATE
    project_options
    project_warnings
    PUBLIC
    fmt::fmt)

if(NOT BUILD_SHARED_LIBS)
    set_property(TARGET alisp_streams PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

install(TARGETS alisp_streams
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
