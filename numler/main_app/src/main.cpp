#include <iostream>

#include <vector>
#include <string>
#include <algorithm>

#include <SFML/Graphics.hpp>

#include "numler/config.hpp"
#include "numler/common/utils.hpp"
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
        
    }

    void render()
    {
        
    }
    
};


int main(int argc, char* argv[])
{

    App app(argc, argv);

    return app.run();
}
