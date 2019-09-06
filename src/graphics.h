#ifndef GRAPHICS_H_
#define GRAPHICS_H_
#include <SDL2/SDL.h>
#include <string>

struct Graphics
{
	Graphics();
	~Graphics();

	void draw_sprite(
			SDL_Texture* src,
			SDL_Rect* src_rect,
			SDL_Rect* dst_rect
			);

	SDL_Renderer* renderer; //The renderer that will be used for textures
	
	private:
	SDL_Window* window; //The window we'll be rendering to
	SDL_Texture* bg;
	std::string title{"2hu"}; //Window title
	const int SCREEN_WIDTH = 640;
	const int SCREEN_HEIGHT = 640;
};
#endif
