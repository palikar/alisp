add_subdirectory(utility)
add_subdirectory(applications)
add_subdirectory(streams)
add_subdirectory(management)
add_subdirectory(alisp)

if(NOT DISABLE_DYN_MODULES)
    add_subdirectory(modules)
endif()
