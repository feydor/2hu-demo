#include "rect.h"

SDL_Rect floatrect_to_sdlrect(FloatRect *r) {
    return (SDL_Rect){.x = r->x, .y = r->y, .w = r->w, .h = r->h};
}

void floatrect_print(FloatRect *r) {
    printf("FloatRect{.x=%f, .y=%f, .w=%f, .h=%f}\n", r->x, r->y, r->w, r->h);
}
