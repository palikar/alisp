add_executable(alcpp
    src/alcppcompiler.cpp
    )

target_include_directories(alcpp
    PUBLIC include
    PRIVATE src)

target_link_libraries(alcpp
    PRIVATE
    project_options
    project_warnings
    language_options
    PUBLIC
    alisp_util
    alisp_language
    fmt::fmt
    ${READLINE_LIB})


target_compile_definitions(alcpp PRIVATE AL_ROOT=\"${PROJECT_ROOT}\")


# add_executable(templ
#     src/templ.cpp
#     )

# target_include_directories(templ
#     PUBLIC include
#     PRIVATE src)

# target_link_libraries(templ
#     PRIVATE
#     project_options
#     project_warnings
#     language_options
#     PUBLIC
#     alisp_util
#     alisp_language
#     fmt::fmt
#     ${READLINE_LIB})


add_executable(alisp
    src/alisp.cpp
    src/prompt.cpp)

target_include_directories(alisp
    PUBLIC include
    PRIVATE src)

target_link_libraries(alisp
    PRIVATE
    project_options
    project_warnings
    language_options
    PUBLIC
    alisp_util
    alisp_language
    fmt::fmt
    ${READLINE_LIB})



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
