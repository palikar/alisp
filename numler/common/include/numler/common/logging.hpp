#pragma once

#include <spdlog/spdlog.h>
#include <spdlog/sinks/stdout_color_sinks.h>
#include <spdlog/sinks/rotating_file_sink.h>
#include <spdlog/sinks/basic_file_sink.h>
#include <spdlog/sinks/null_sink.h>
#include <fmt/time.h>

#include <memory>
#include <string>
#include <vector>
#include <iterator>

#include "numler/config.hpp"


namespace nu::logging
{


    spdlog::logger* get_main_logger();
    spdlog::logger* get_dev_logger();
    
    void init_logging(bool debug = false);

    std::string get_time_str();
}


#if DEBUG_LOGGING
#define DEBUG(...) nu::logging::get_dev_logger->debug(__VA_ARGS__)
#else
#define DEBUG(...) (void)0
#endif

#define ABORT(...) \
    do {           \
        nu::logging::get_main_logger->critical(__VA_ARGS__);       \
        nu::logging::get_main_logger->critical("Aborting!");       \
        abort();                                                   \
    } while (0)
