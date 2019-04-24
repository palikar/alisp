#include <iostream>

#include <vector>
#include <string>
#include <algorithm>

#include <SFML/Graphics.hpp>
#include <spdlog/spdlog.h>
#include <spdlog/sinks/basic_file_sink.h>

#include "numler/config.hpp"
#include "numler/common/utils.hpp"
#include "numler/common/logging.hpp"
#include "numler/main_app/framework.hpp"






class App : public nu::GraphingFramework<App>
{
private:
    
public:

    App(int argc, char* argv[]):GraphingFramework(argc, argv)
    {
    }
    
    void update()
    {
        // spdlog::info("Updating");
    }

    void render()
    {
        // spdlog::info("Rendering");
    }
    
};


int main(int argc, char* argv[])
{
    nu::logging::init_logging();

    DEBUG("This is the shit man! {}", 42);

    return 0;
    
    // App app(argc, argv);

    // return app.run();
}
