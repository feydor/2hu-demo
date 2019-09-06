#include "graphics.h"
#include <SDL2/SDL_image.h>
#include <iostream>

Graphics::Graphics()
{
	//Create and init window
	window = SDL_CreateWindow(
			title.c_str(),					// title
			SDL_WINDOWPOS_UNDEFINED,	// init xpos
			SDL_WINDOWPOS_UNDEFINED,	// init ypos
			SCREEN_WIDTH,				// width
			SCREEN_HEIGHT,			// height
			SDL_WINDOW_SHOWN			// flags
			);
	if(window == nullptr)
	{
		std::cout << "Window could not be created! SDL_Error: " << SDL_GetError() << std::endl;
	}
	
	//Create and init renderer
	renderer = SDL_CreateRenderer( window, -1, SDL_RENDERER_ACCELERATED );
	if ( renderer == nullptr )
	{
		std::cout << "Failed to create renderer : " << SDL_GetError();
	}
	// Set size of renderer to the same as window
	SDL_RenderSetLogicalSize( renderer, SCREEN_WIDTH, SCREEN_HEIGHT );
	// Set color of renderer to black
	SDL_SetRenderDrawColor( renderer, 0, 0, 0, 0 );
	
	//Initialize PNG loading
	int imgFlags = IMG_INIT_PNG;
	if(!(IMG_Init(imgFlags) & imgFlags))
	{
		std::cout << "SDL_image could not initialize! SDL_image Error: " << IMG_GetError();
		std::cout << std::endl;
	}
}

void Graphics::draw_sprite(
		SDL_Texture* src,
		SDL_Rect* src_rect,
		SDL_Rect* dst_rect
		)
{
	SDL_RenderCopy(renderer, src, src_rect, dst_rect);
}

Graphics::~Graphics()
{
	//Destroy renderer
	SDL_DestroyRenderer(renderer);
	//Destroy window
	SDL_DestroyWindow(window);
}

// returns a loaded texture from given input file
/*SDL_Texture* load_texture(std::string fname)
{
	//Load image as SDL_Surface
	SDL_Surface* surface = IMG_Load(fname.c_str());
	if(surface == nullptr)
	{
		std::cout << "Failed to load surface at " << fname
		<< " error: " << IMG_GetError() << std::endl; 
		return nullptr;
	}
	 
    // SDL_Surface is just the raw pixels
    // Convert it to a hardware-optimzed texture so we can render it
	texture = SDL_CreateTextureFromSurface(renderer, surface);

	//Release surface
	SDL_FreeSurface(surface);

	return texture;
}*/

/*SDL_Texture* print_font(std::string text)
{
	surf = TTF_RenderText_Solid( font, text.c_str(), textColor );

	font_texture = SDL_CreateTextureFromSurface( renderer, surf );
	SDL_FreeSurface(surf);

	return font_texture;
}*/

//Draws with renderer
/*void draw()
{
	// Clear the window and make it all red
    SDL_RenderClear( renderer );
	
	SDL_RenderCopy(renderer, texture, NULL, NULL);

	//SDL_RenderFillRect( renderer, &player_pos );

    // Render the changes above ( which up until now had just happened behind the scenes )
    SDL_RenderPresent( renderer);
}*/
