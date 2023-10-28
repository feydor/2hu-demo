#ifndef TWOHU_H
#define TWOHU_H
#include <SDL2/SDL.h>

typedef struct Twohu Twohu;

Twohu *create_twohu(SDL_Renderer *renderer, SDL_Window *window);
void twohu_event(Twohu *th, SDL_Event *e);
void twohu_update(Twohu *th, float dt);
void twohu_render(Twohu *th);
#endif