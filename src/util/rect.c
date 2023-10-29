#include "rect.h"

SDL_Rect floatrect_to_sdlrect(FloatRect *r) {
    return (SDL_Rect){.x = r->x, .y = r->y, .w = r->w, .h = r->h};
}

void floatrect_print(FloatRect *r) {
    printf("FloatRect{.x=%f, .y=%f, .w=%f, .h=%f}\n", r->x, r->y, r->w, r->h);
}

bool floatrect_are_colliding(const FloatRect *rect1, const FloatRect *rect2) {
    // Check collision along the x-axis
    if (rect1->x < rect2->x + rect2->w && rect1->x + rect1->w > rect2->x) {
        // Check collision along the y-axis
        if (rect1->y < rect2->y + rect2->h && rect1->y + rect1->h > rect2->y) {
            // Rectangles overlap along both axes, collision detected
            return true;
        }
    }

    return false;
}
