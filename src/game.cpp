#include "game.h"

//static
int Game::k_tile_height = 50;
int Game::k_tile_width = 32;

Game::Game()
{
	SDL_Init(SDL_INIT_EVERYTHING);
	event_loop();
}

Game::~Game()
{
	SDL_Quit();
}

void Game::event_loop()
{
	Graphics graphics; //SDL
	SDL_Event event;
	
	sprite_ptr.reset(new Sprite("res/reimu.png", 0, 0,
				k_tile_width, k_tile_height, 15, 3));

	int frame_delay = 1000 / 60; //60 fps
	
	bool running = true;
	int last_update_time = SDL_GetTicks();
	while(running)
	{
		const int start_time_ms = SDL_GetTicks(); //initial time
		while(SDL_PollEvent(&event))
		{
			switch(event.type)
			{
				case SDL_KEYDOWN:
					if(event.key.keysym.sym == SDLK_ESCAPE)
					{
						running = false;
					}
					break;
				case SDL_QUIT:
					running = false;
					break;
				default:
					break;
			}
		}
		
		const int curr_time_ms = SDL_GetTicks();
		update(curr_time_ms - last_update_time); //elapsed time since last update
		last_update_time = curr_time_ms;

		draw(graphics);

		const int elapsed_time_ms = SDL_GetTicks() - start_time_ms; //time elapsed by events/draw
		if(frame_delay > elapsed_time_ms)
		{
			SDL_Delay(frame_delay - elapsed_time_ms);
		}
		/*const float seconds_per_frame = (SDL_GetTicks() - start_time_ms) / 1000.0;
		const float fps = 1 / seconds_per_frame;
		std::cout << "fps: " << fps << std::endl;*/
	}
}

void Game::update(int elapsed_time_ms)
{
	sprite_ptr->update(elapsed_time_ms);
}

void Game::draw(Graphics &graphics)
{	
	SDL_RenderClear(graphics.renderer);
	sprite_ptr->draw(graphics, 320, 240);
	// Render the changes above ( which up until now had just happened behind the scenes )
	SDL_RenderPresent( graphics.renderer);
}
