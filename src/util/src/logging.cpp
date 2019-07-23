#include <spdlog/spdlog.h>
#include <spdlog/sinks/stdout_color_sinks.h>
#include <spdlog/sinks/rotating_file_sink.h>
#include <spdlog/sinks/basic_file_sink.h>
#include <spdlog/sinks/null_sink.h>

#include <memory>
#include <string>
#include <vector>
#include <iterator>

#include "alisp/config.hpp"


namespace alisp::logging
{
    
std::shared_ptr<spdlog::logger> _main_logger;
std::shared_ptr<spdlog::logger> _dev_debug;

const std::string format{"[%@][%c][%^%l%$] %v"};

const std::string file_name{"./alisp_log.txt"};

const std::string dev_debug_format{"[%@][%^DEBUG%$] %v"};

  
spdlog::logger* get_main_logger()
{
  return _main_logger.get();
}

spdlog::logger* get_dev_logger()
{
	return _dev_debug.get();
	}
  
void init_logging(bool debug = false)
{

	std::vector<spdlog::sink_ptr> sinks;

#if CONSOLE_LOGGING
	auto console_sink = std::make_shared<spdlog::sinks::stdout_color_sink_mt>();
	console_sink->set_pattern(format);
	console_sink->set_level(spdlog::level::debug);
	sinks.push_back(console_sink);
#endif
        
        
#if FILE_LOGGING
    auto rotating_sink = std::make_shared<spdlog::sinks::rotating_file_sink_mt>(file_name, 1024*1024, 10);
    rotating_sink->set_pattern(format);
    rotating_sink->set_level(spdlog::level::trace);
    sinks.push_back(rotating_sink);
#endif



    _main_logger = std::make_shared<spdlog::logger>("main", std::begin(sinks), std::end(sinks));
    if (debug)
    {
			_main_logger->set_level(spdlog::level::debug);
    }


#if DEBUG_LOGGING
    auto debug_sink = std::make_shared<spdlog::sinks::stdout_color_sink_mt>();
    debug_sink->set_pattern(dev_debug_format);
    debug_sink->set_level(spdlog::level::trace);

    _dev_debug = std::make_shared<spdlog::logger>("dev", debug_sink);
    _dev_debug->set_level(spdlog::level::trace);
    spdlog::register_logger(_dev_debug);
#endif

        


    spdlog::register_logger(_main_logger);
        
    spdlog::set_default_logger(_main_logger);

        
	}
}

