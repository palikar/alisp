
#include "numler/config.hpp"
#include "numler/common/utils.hpp"
#include "numler/main_app/framework.hpp"

#include <SFML/Graphics.hpp>
#include <spdlog/spdlog.h>

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>




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
    

    App app(argc, argv);

    
    return app.run();
}
