#ifndef GAME_H_
#define GAME_H_
#include <SDL2/SDL.h>
#include <iostream>
#include <memory>
#include "graphics.h"
#include "sprite.h"

struct Game
{
	Game();
	~Game();

	static int k_tile_height;
	static int k_tile_width;

	private:
	 void event_loop();
	 void update(int elapsed_time_ms);
	 void draw(Graphics &graphics);

	 std::unique_ptr<Sprite> sprite_ptr; 
};
#endif
