#ifndef ENTITY_H
#define ENTITY_H
#include "../bullet/bullet.h"
#include <SDL2/SDL.h>
#include <stdbool.h>

//typedef struct TwohuEntity TwohuEntity;

typedef struct TwohuSpritesheetManager {
    int curr_anim;
    int curr_frame;
    int n_anims;
    /** The dimensions of a single sprite */
    int sprite_w, sprite_h;
    SDL_Rect clip;
    SDL_Surface *image;
    SDL_Texture *texture;
} TwohuSpritesheetManager;

typedef struct FloatRect {
    float x, y, w, h;
} FloatRect;

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

SDL_Rect floatrect_to_sdlrect(FloatRect *r);
void floatrect_print(FloatRect *r);

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