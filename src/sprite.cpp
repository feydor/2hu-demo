#include "sprite.h"
#include "game.h"

Sprite::Sprite(const std::string &fname,
		int src_x, int src_y,
		int w, int h)
	:frame_time(0),
	num_frames(0),
	curr_frame(0),
	elapsed_time(0)
{
	//Load image as SDL_Surface
	sprite_sheet = IMG_Load(fname.c_str());
	if(sprite_sheet == nullptr)
	{
		std::cout << "Failed to load surface at " << fname
		<< " error: " << IMG_GetError() << std::endl;
	}
	
	// set up src_rect or hit box
	src_rect.x = src_x;
	src_rect.y = src_y;
	src_rect.h = h;
	src_rect.w = w;

	this->h = h;
	this->w = w;
}
//animation version
Sprite::Sprite(const std::string &fname,
		int src_x, int src_y,
		int w, int h,
		int fps, int num_frames)
	:Sprite(fname, src_x, src_y, w, h)
{
	frame_time = (1000 / fps);
	this->num_frames = num_frames;
}

void Sprite::draw(Graphics &graphics, int x, int y)
{
	SDL_Rect dst_rect;
	dst_rect.x = x;
	dst_rect.y = y;
	//width of sprite
	dst_rect.h = h;
	dst_rect.w = w;

	//converts sprite_sheet surface into texture, neccesary for next step
	text = SDL_CreateTextureFromSurface(graphics.renderer, sprite_sheet);
	//SDL_FreeSurface(sprite_sheet);
	graphics.draw_sprite(text, &src_rect, &dst_rect);
}

//animation
void Sprite::update(int elapsed_time_ms)
{
	elapsed_time += elapsed_time_ms;
	if(elapsed_time > frame_time)
	{
		++curr_frame;
		elapsed_time = 0;
		if(curr_frame < num_frames) //normal case
		{
			src_rect.x += Game::k_tile_width;
		}
		else						//beyond last frame
		{
			src_rect.x -= Game::k_tile_width * (num_frames - 1);
			curr_frame = 0;
		}
	}
}

Sprite::~Sprite()
{
	SDL_DestroyTexture(text);
	SDL_FreeSurface(sprite_sheet);
}

