#include <iostream>

#include <vector>
#include <string>

#include "numler/config.hpp"

#include "numler/common/utils.hpp"

#include "SFML/Graphics.hpp"





int main()
{


	std::vector<double> nums(100);

	nu::utils::iota(std::begin(nums), std::end(nums),
					nu::utils::Inc(0.0, 0.2));

	for (const auto &v : nums) { std::cout << v << "\n"; };

2

//sf::RenderWindow renderWindow(sf::VideoMode(640, 480), "Hello Cruel World");

	// while (renderWindow.isOpen()){
	// 	sf::Event event;

	// 	while (renderWindow.pollEvent(event)){
	// 		if (event.type == sf::Event::EventType::Closed)
	// 		{
	// 			renderWindow.close();
	// 		}

			
			
	// 	}

		
	// 	Renderwindow.clear(sf::Color::White);
	// 	renderWindow.display();
    // }
	
}
