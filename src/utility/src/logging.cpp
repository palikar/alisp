#include "alisp/utility/logging.hpp"

namespace alisp::logging
{

void init_logging([[maybe_unused]] bool debug,  [[maybe_unused]] bool stand) {

#ifdef DEUBG_LOGGING
    LOGGER = standard_logger(debug, stand);
#endif
}

}


