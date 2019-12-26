#pragma once

#include <memory>
#include <string>
#include <vector>
#include <iterator>
#include <string>
#include <ostream>
#include <sstream>
#include <functional>
#include <iostream>

// #include "alisp/config.hpp"


namespace alisp::logging
{


enum class LogCategory
{
    ANY,
    DEBUG,
    INFO,
    WARNING,
    ERROR
};


struct LogEntry
{
    std::string file_name; 
    std::string function_name; 
    int line_number; 
    std::stringstream message;
};


template <LogCategory t_category>
class StdLog
{
    static const LogCategory category = t_category;

    void log(LogEntry t_entry) {
        std::cout << '[' << t_entry.file_name << ':' << t_entry.function_name << ':' << t_entry.line_number << ']'
                  <<  t_entry.message.str ( ) << "\n";
    }
};

class NoopLog
{
    static const LogCategory category = LogCategory::ANY;

    void log(LogEntry) {}
};


template <LogCategory t_category>
class ErrLog
{
    static const LogCategory category = t_category;

    void log(LogEntry t_entry) {
        std::cerr << '[' << t_entry.file_name << ':' << t_entry.function_name << ':' << t_entry.line_number << ']'
                  <<  t_entry.message.str ( ) << "\n";
    }
};


template<typename ... Logs>
class LogKeeper : Logs ...
{
  private:
    bool m_debug = false;
    bool m_standard = false;
    
  public:

    LogKeeper(bool t_debug = false, bool t_standard = true)
        :m_debug(t_debug), m_standard(t_standard) {}

    template<typename MSG>
        void create_entry(MSG msg, const char* file, const char* function, int line_number, LogCategory category) {
        LogEntry entry{std::string(file), std::string(function), line_number, msg};
     
        (do_log(static_cast<Logs&>(*this), entry, category), ...);
    }


    template<typename Logger>
        void do_log(Logger && logger, LogEntry t_entry, LogCategory t_category) {
        
        if ( Logger::category == t_category || Logger::category == LogCategory::ANY ) {

            if (!m_debug and t_category == LogCategory::DEBUG){ return; }
            if (!m_standard and t_category != LogCategory::DEBUG){ return; }
            

            logger.log(t_entry);
        }

        
    }

    
    
};




#ifdef DEUBG_LOGGING
using standard_logger = LogKeeper<
    StdLog<LogCategory::DEBUG>,
    StdLog<LogCategory::INFO>,
    StdLog<LogCategory::WARNING>,
    ErrLog<LogCategory::ERROR>
    >;
standard_logger LOGGER;

#define AL_DEBUG(MSG)   do{ ::alisp::logging::LOGGER.create_entry(MSG, __FILE__, __FUNCTION__, __LINE__, LogCategory::DEBUG); }while(false)
#define AL_INFO(MSG)    do{ ::alisp::logging::LOGGER.create_entry(MSG, __FILE__, __FUNCTION__, __LINE__, LogCategory::INFO); }while(false)
#define AL_WARNING(MSG) do{ ::alisp::logging::LOGGER.create_entry(MSG, __FILE__, __FUNCTION__, __LINE__, LogCategory::WARNING); }while(false)
#define AL_ERROR(MSG)   do{ ::alisp::logging::LOGGER.create_entry(MSG, __FILE__, __FUNCTION__, __LINE__, LogCategory::ERROR); }while(false)

#else
using standard_logger = LogKeeper<NoopLog>;

#define AL_DEBUG(MSG)    do{(void)0;}while(false)
#define AL_INFO(MSG)     do{(void)0;}while(false)
#define AL_WARNING(MSG)  do{(void)0;}while(false)
#define AL_ERROR(MSG)    do{(void)0;}while(false)

#endif

void init_logging(bool debug =  false, bool stand = true);




}


