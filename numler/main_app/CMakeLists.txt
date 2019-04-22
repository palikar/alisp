add_executable(numler_main_app
	src/main.cpp
	)


target_include_directories(numler_main_app
	PUBLIC include
	PRIVATE src
	)

## If you want to link SFML statically

## In most cases better set in the CMake cache
# set(SFML_DIR "<sfml root prefix>/lib/cmake/SFML")

# set(SFML_STATIC_LIBRARIES TRUE)
# find_package(SFML 2.5 COMPONENTS graphics audio REQUIRED)
# find_package(SFML 2.5 REQUIRED network audio graphics window system)

target_link_libraries(numler_main_app
	PRIVATE
	project_options
	project_warnings
	numler_common
	PUBLIC
	Threads::Threads
	fmt::fmt
	spdlog::spdlog
	imgui
	
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
