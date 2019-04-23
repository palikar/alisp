/**
 * @file   framework.cpp
 * @author Stanislav Arnaudov <stanislav.arn@gmail.com>
 * @date   Tue Apr 23 21:04:23 2019
 * 
 * @brief  
 * 
 * 
 */
#pragma once
#include "numler/common/meta.hpp"

#include <SFML/Graphics.hpp>
#include <clara.hpp>

#include <iostream>
#include <memory>
#include <atomic>
#include <filesystem>
#include <string>

namespace nu{

    struct Options
    {
        int width = 640;
        int height = 480;
        bool gui = true;
    };

    template <typename T>
    class GraphingFramework : public meta::crtp<T>
    {
    public:

        GraphingFramework(int argc, char* argv[]):
            running(false)
        {
            using clara::Opt;
            using clara::Arg;
            using clara::Args;
            using clara::Help;

            bool showHelp{ false };

            auto cli = Help(showHelp)
                | Opt(this->opts.width, "int")["-w"]["--width"]("The width of the created window")
                | Opt(this->opts.height, "int")["-h"]["--height"]("The height of the created window")
                | Opt(this->opts.gui)["-g"]["--gui"]("Show GUI or not");
                

            const auto result = cli.parse(Args(argc, argv));

            if (!result) {
                std::cerr << "Error in command line: " << result.errorMessage() << '\n';
            }
            
            if (showHelp) {
                std::cout << cli << '\n';
                exit(1);
            }


            
            this->window = std::make_unique<sf::RenderWindow>(sf::VideoMode(opts.width, opts.height), "GraphingWorks");
            
        }
        

        int run()
        {
            
            running = true;
            
            while(running && this->window->isOpen())
            {
                sf::Event event;
                while (this->window->pollEvent(event))
                {

                    if (event.type == sf::Event::EventType::Closed)
                    {
                        running = false;
                        this->window->close();
                    }
                
                }
                
                this->window->clear(sf::Color::White);

                this->underlying().update();
                this->underlying().render();

                this->window->display();
            }

            
            
            return 0;
        }

        void stop()
        {
            running = false;
        }
        
        virtual ~GraphingFramework(){
            
        }
    private:
        Options opts;
        std::atomic<bool> running;
        std::unique_ptr<sf::RenderWindow> window;
        
    };

    
}
