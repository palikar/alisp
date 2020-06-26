add_library(tinyxml2 tinyxml2.cpp)
if(NOT BUILD_SHARED_LIBS)
    set_property(TARGET tinyxml2 PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()
install(TARGETS tinyxml2
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )


set(FMT_PEDANTIC ON)
set(FMT_INSTALL ON)
add_subdirectory(fmt)


set(BUILD_SHARED_LIBS ON)
set(USE_SYSTEM_CURL ON)
set(BUILD_CPR_TESTS OFF)



if(${CMAKE_VERSION} VERSION_GREATER "3.16.0")
    FetchContent_Declare(cpr
        GIT_REPOSITORY https://github.com/whoshuu/cpr.git
        GIT_TAG a78ce2655a0f3d8d72fd0ee57f950105666b96f8)
    FetchContent_MakeAvailable(cpr)
endif()



set(HTTPLIB_COMPILE ON)
add_subdirectory(httplib)


add_library(usockets STATIC

    ./uSockets/context.c
    ./uSockets/socket.c
    ./uSockets/loop.c
    
    ./uSockets/eventing/epoll_kqueue.c
    ./uSockets/eventing/gcd.c
    ./uSockets/eventing/libuv.c
    
    ./uSockets/crypto/openssl.c
    # ./uSockets/crypto/wolfssl.c
    
    )
target_include_directories(usockets PUBLIC ./uSockets)

target_compile_definitions(usockets PUBLIC LIBUS_NO_SSL)
# target_compile_definitions(usockets PUBLIC LIBUS_USE_OPENSSL=1)
target_link_libraries(usockets PUBLIC -flto -lz  -lssl -lcrypto )

set_target_properties(usockets PROPERTIES LANGUAGE CXX)
set_target_properties(usockets PROPERTIES LINKER_LANGUAGE CXX)
set_target_properties(usockets PROPERTIES POSITION_INDEPENDENT_CODE ON)



install(TARGETS usockets
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    RUNTIME DESTINATION       ${CMAKE_INSTALL_BINDIR}
    LIBRARY DESTINATION       ${CMAKE_INSTALL_LIBDIR}
    INCLUDES DESTINATION      ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/
    ARCHIVE DESTINATION       ${CMAKE_INSTALL_SHAREDSTATEDIR}
    )



add_library(uwebsockets INTERFACE)
target_link_libraries(uwebsockets INTERFACE usockets)
target_include_directories(uwebsockets INTERFACE ./uWebSockets/)


set(BUILD_SSL OFF)
set(BUILD_TESTS OFF)
add_subdirectory(restbed)
