/*   Alisp - the alisp interpreted language
     Copyright (C) 2020 Stanislav Arnaudov

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2 of the License, or
 (at your option) any prior version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along
 with this program; if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. */

#pragma once

#include <iostream>
#include <string>
#include <cstring>

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
    std::string message;
};

template<LogCategory t_category> struct StdLog
{
    static const LogCategory category = t_category;

    void log(LogEntry t_entry)
    {
        std::cout << '[' << t_entry.file_name << ':' << t_entry.function_name << ':' << t_entry.line_number << "] " << t_entry.message << "\n";
    }
};

struct NoopLog
{
    static const LogCategory category = LogCategory::ANY;

    void log(LogEntry) {}
};

template<LogCategory t_category> struct ErrLog
{
    static const LogCategory category = t_category;

    void log(LogEntry t_entry)
    {
        std::cerr << '[' << t_entry.file_name << ':' << t_entry.function_name << ':' << t_entry.line_number << "] " << t_entry.message << "\n";
    }
};

template<typename... Logs> class LogKeeper : Logs...
{
  private:
    bool m_debug    = false;
    bool m_standard = false;

  public:
    LogKeeper(bool t_debug = false, bool t_standard = true) : m_debug(t_debug), m_standard(t_standard) {}

    template<typename MSG> void create_entry(MSG msg, const char *file, const char *function, int line_number, LogCategory category)
    {
        const char *last_slash = strrchr(file, '/');
        last_slash             = last_slash ? last_slash + 1 : file;

        LogEntry entry{ std::string(last_slash), std::string(function), line_number, msg };

        (do_log(static_cast<Logs &>(*this), entry, category), ...);
    }


    template<typename Logger> void do_log(Logger &&logger, LogEntry t_entry, LogCategory t_category)
    {

        if (logger.category == t_category || logger.category == LogCategory::ANY)
        {

            if (!m_debug and t_category == LogCategory::DEBUG) { return; }
            if (!m_standard and t_category != LogCategory::DEBUG) { return; }


            logger.log(t_entry);
        }
    }
};


#ifdef DEUBG_LOGGING
using standard_logger = LogKeeper<StdLog<LogCategory::DEBUG>, StdLog<LogCategory::INFO>, StdLog<LogCategory::WARNING>, ErrLog<LogCategory::ERROR>>;
inline standard_logger LOGGER;

#define AL_LOG(MSG, TAG)                                                                                                  \
    do                                                                                                                    \
    {                                                                                                                     \
        ::alisp::logging::LOGGER.create_entry(MSG, __FILE__, __FUNCTION__, __LINE__, ::alisp::logging::LogCategory::TAG); \
    } while (false)
#else

using standard_logger = LogKeeper<NoopLog>;

#define AL_LOG(MSG, TAG) \
    do                   \
    {                    \
        (void)0;         \
    } while (false)

#endif


#define AL_DEBUG(MSG) AL_LOG(MSG, DEBUG)
#define AL_INFO(MSG) AL_LOG(MSG, INFO)
#define AL_WARNING(MSG) AL_LOG(MSG, WARNING)
#define AL_ERROR(MSG) AL_LOG(MSG, ERROR)

void init_logging(bool debug = false, bool stand = true);


}  // namespace alisp::logging
