#ifndef INC_2HU_DEMO_RECT_H
#define INC_2HU_DEMO_RECT_H
#include <SDL2/SDL.h>

/** A rectangle of floats */
typedef struct FloatRect {
    float x, y, w, h;
} FloatRect;

SDL_Rect floatrect_to_sdlrect(FloatRect *r);
void floatrect_print(FloatRect *r);

#endif // INC_2HU_DEMO_RECT_H
