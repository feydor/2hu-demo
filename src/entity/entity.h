#ifndef ENTITY_H
#define ENTITY_H
#include "../bullet/bullet.h"
#include "../util/rect.h"
#include "spritesheet_manager.h"
#include <SDL2/SDL.h>
#include <stdbool.h>

typedef struct TwohuEntity {
    FloatRect rect;
    SDL_Point hitbox;
    SDL_Surface *surface;
    TwohuSpritesheetManager sheet_manager;
    TwohuBulletManager bullet_manager;
    int nsheets;
    float dx, dy;
    float speed;
    bool player;
    bool alive;
} TwohuEntity;

TwohuEntity create_twohu_player(FloatRect rect, SDL_Point hitbox);
TwohuEntity create_twohu_enemy(FloatRect rect, SDL_Point hitbox);
void twohu_entity_event(TwohuEntity *self, SDL_Event *e);
void twohu_entity_update(TwohuEntity *self, float dt);
void twohu_entity_render(TwohuEntity *self, SDL_Renderer *renderer);

// inline helper functions

float twohu_W(TwohuEntity *entity);
float twohu_H(TwohuEntity *entity);
SDL_Point twohu_entity_center(TwohuEntity *e);

#endif