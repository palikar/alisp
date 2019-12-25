#pragma once
#include <memory>
#include <string>
#include <vector>
#include <iterator>

#include "alisp/config.hpp"


namespace alisp::logging
{


}


#if DEBUG_LOGGING
#define DEBUG(...) alisp::logging::get_dev_logger()->debug(__VA_ARGS__)
#else
#define DEBUG(...) (void)0
#endif

#define ABORT(...)                                                  \
	do {                                                            \
		alisp::logging::get_main_logger()->critical(__VA_ARGS__);   \
		alisp::logging::get_main_logger()->critical("Aborting!");   \
		abort();                                                    \
	} while (0)
 
