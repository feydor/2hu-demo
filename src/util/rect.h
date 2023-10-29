#ifndef INC_2HU_DEMO_RECT_H
#define INC_2HU_DEMO_RECT_H
#include <SDL2/SDL.h>
#include <stdbool.h>

/** A rectangle of floats */
typedef struct FloatRect {
    float x, y, w, h;
} FloatRect;

SDL_Rect floatrect_to_sdlrect(FloatRect *r);
void floatrect_print(FloatRect *r);
bool floatrect_are_colliding(const FloatRect *rect1, const FloatRect *rect2);

#endif // INC_2HU_DEMO_RECT_H
