#ifndef ENTITY_H
#define ENTITY_H
#include <SDL2/SDL.h>

typedef struct TwohuEntity TwohuEntity;

typedef struct FloatRect {
    float x, y, w, h;
} FloatRect;

TwohuEntity *create_twohu_entity(FloatRect rect, SDL_Point hitbox);
void twohu_entity_event(TwohuEntity *entity, SDL_Event *e);
void twohu_entity_update(TwohuEntity *entity, float dt);
void twohu_entity_render(TwohuEntity *entity, SDL_Renderer *renderer);

#endif