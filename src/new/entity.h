#ifndef ENTITY_H
#define ENTITY_H
#include <SDL2/SDL.h>

typedef struct TwohuEntity TwohuEntity;

typedef struct FloatRect {
    float x, y, w, h;
} FloatRect;

TwohuEntity *create_twohu_player(FloatRect rect, SDL_Point hitbox);
TwohuEntity *create_twohu_enemy(FloatRect rect, SDL_Point hitbox);
void twohu_entity_event(TwohuEntity *entity, SDL_Event *e);
void twohu_entity_update(TwohuEntity *entity, float dt);
void twohu_entity_render(TwohuEntity *entity, SDL_Renderer *renderer);

// inline helper functions

float twohu_W(TwohuEntity *entity);
float twohu_H(TwohuEntity *entity);

#endif