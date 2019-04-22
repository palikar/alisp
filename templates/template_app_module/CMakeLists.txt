add_executable(numler_MODULE_NAME
	src/main.cpp
	)


target_include_directories(numler_MODULE_NAME
	PUBLIC include
	PRIVATE src
	)

target_link_libraries(numler_MODULE_NAME
    PRIVATE
	project_options
    project_warnings
	PUBLIC
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
