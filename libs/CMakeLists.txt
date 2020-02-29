add_library(tinyxml2 tinyxml2.cpp)
if(NOT BUILD_SHARED_LIBS)
    set_property(TARGET tinyxml2 PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()


