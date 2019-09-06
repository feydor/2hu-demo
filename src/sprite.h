#ifndef SPRITE_H_
#define SPRITE_H_
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <string>
#include <iostream>
#include "graphics.h"
//#include "game.h"

struct Sprite
{
	Sprite(const std::string &fname,
			int src_x, int src_y,
			int w, int h);
	//animated sprite
	Sprite(const std::string &fname,
			int src_x, int src_y,
			int w, int h,
			int fps, int num_frames);
	~Sprite();
	
	void draw(Graphics &graphics, int x, int y);

	void update(int elapsed_time_ms);

	private:
		int h;
		int w;
		SDL_Surface* sprite_sheet;
		SDL_Rect src_rect;
		SDL_Texture* text; //proxy for sprite_sheet
		
		int frame_time; //ms per frame
		int num_frames;
		int curr_frame;
		int elapsed_time; //time since last frame change
};
#endif
