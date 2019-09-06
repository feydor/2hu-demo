#include <iostream>
#include "game.h"

void show_usage_and_exit()
{
	std::cout << "usage: " << " 2hu " << "[input_file] [-h]" << std::endl; 
	exit(-1);
}

int main(int argc, char* argv[]) {
	/*if(argc == 1)
	{
		load_texture("res/rewire.png");
	} 
	else if(argc == 2)
	{
		std::string arg1(argv[1]);
		if(arg1 == "-h")
		{
			show_usage_and_exit();
		}
		load_texture(arg1);
	}
	else
	{
		show_usage_and_exit();
	}

	print_font("noumenon");
	*/
	Game game;	
	return 0;	
}
