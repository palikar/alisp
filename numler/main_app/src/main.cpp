#include <iostream>

#include <vector>
#include <string>
#include <algorithm>

#include <SFML/Graphics.hpp>

#include "numler/config.hpp"
#include "numler/common/utils.hpp"




int main()
{


	constexpr int points = 1000;
	std::vector<double> nums(points);

	nu::utils::iota(std::begin(nums), std::end(nums),
					nu::utils::IncWithFact(0.0, 1.0/points, 5.));
	
	
	

	for (const auto &v : nums) { std::cout << v << "\n"; };


	
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
