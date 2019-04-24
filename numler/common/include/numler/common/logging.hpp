
#include <spdlog/spdlog.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include <memory>


namespace nu::logging
{

    std::shared_ptr<spdlog::logger> _logger;
    
    
    void init_logging()
    {


        auto console_sink = std::make_shared<spdlog::sinks::stdout_color_sink_mt>();
        
        console_sink->set_pattern("[%@][%c][%^%l%$] %v");

        
        
        spdlog::logger logger("main_logger", {console_sink});
        // logger.set_level(spdlog::level::debug);

        logger.warn("this should appear in both console and file");
        logger.info("this message should not appear in the console, only in the file");
        

    }

}
