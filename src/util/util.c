#include "util.h"
#include <SDL2/SDL_image.h>

SDL_Surface *load_surface_or_exit(const char *path) {
    SDL_Surface *surface = IMG_Load(path);
    if (!surface) {
        exit(fprintf(stderr, "Failed to load the image: '%s'!\n", path));
    }
    return surface;
}

SDL_Texture *load_texture_or_exit(SDL_Renderer *renderer, SDL_Surface *surface) {
    SDL_Texture *texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!texture) {
        exit(fprintf(stderr, "Failed to create texture!\n"));
    }
    return texture;
}
