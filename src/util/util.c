#include "util.h"
#include <SDL2/SDL_image.h>

SDL_Surface *load_surface_or_exit(const char *path) {
    IMG_Init(IMG_INIT_PNG);
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

/** Returns the gradient between two points in x and in y */
SDL_FPoint gradient(SDL_Point self, SDL_Point other) {
    int steps = MAX(abs(self.x - other.x), abs(self.y - other.y));

    if (steps == 0) {
        return (SDL_FPoint){0, 0};
    }

    SDL_FPoint g;
    g.x = self.x - other.x;
    g.x /= steps;

    g.y = self.y - other.y;
    g.y /= steps;

    return g;
}