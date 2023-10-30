#ifndef INC_2HU_DEMO_UTIL_H
#define INC_2HU_DEMO_UTIL_H
#include <SDL2/SDL.h>
#define MAX(X, Y) (((X) > (Y)) ? (X) : (Y))
#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))

/**
 * Loads the SDL surface with the image at path or exits
 * @param path an image supported by SDL image (png, bmp, gif, etc)
 * @return new surface
 */
SDL_Surface *load_surface_or_exit(const char *path);

/**
 * Loads the SDL texture with the surface or exists
 * @return a new texture
 */
SDL_Texture *load_texture_or_exit(SDL_Renderer *renderer, SDL_Surface *surface);

/** Returns the gradient between two points in x and in y */
SDL_FPoint gradient(SDL_Point self, SDL_Point other);

#endif // INC_2HU_DEMO_UTIL_H
