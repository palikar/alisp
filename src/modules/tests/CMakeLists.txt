add_executable(alisp_modules_test
    main_test.cpp

    base64_test.cpp
    fmt_test.cpp
    func_test.cpp
    json_test.cpp
    locale_test.cpp
    process_test.cpp
    random_test.cpp
    re_test.cpp
    xml_test.cpp

    ../src/base64.cpp
    ../src/fmt.cpp
    ../src/func.cpp
    ../src/json.cpp
    ../src/locale.cpp
    ../src/process.cpp
    ../src/random.cpp
    ../src/re.cpp
    ../src/xml.cpp

    )

target_include_directories(alisp_modules_test
    PUBLIC ../include
    PRIVATE ../src
    )

target_link_libraries(alisp_modules_test
    PRIVATE
    project_options
    project_warnings
    language_options
    PUBLIC
    alisp_language
    tinyxml2
    )


target_link_libraries(alisp_modules_test PUBLIC cpr)
target_link_libraries(alisp_modules_test PUBLIC uwebsockets)
target_link_libraries(alisp_modules_test PUBLIC alisp_management)
target_link_libraries(alisp_modules_test PUBLIC tinyxml2)
target_link_libraries(alisp_modules_test PUBLIC uwebsockets)

include(CTest)
include(Catch)
catch_discover_tests(alisp_modules_test)
