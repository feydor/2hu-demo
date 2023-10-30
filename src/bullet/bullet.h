#ifndef TWOHU_BULLET_H
#define TWOHU_BULLET_H
#include <SDL2/SDL.h>
#include <stdbool.h>
#include "../util/rect.h"
#include "../entity/entity.h"

typedef struct {
    FloatRect rect;
    SDL_Point hitbox;
    SDL_Surface *surface;
    float dx, dy;
    float speed;
    bool alive;
} TwohuBullet;

#define BULLET_PNG "./res/marisa-shot.png"
#define MAX_BULLETS 100 /* The size of the circular buffer */
#define BULLET_W 28
#define BULLET_H 62
#define BULLET_INIT_SPEED 3

/** retrieve the global bullet manager */
TwohuBullet *twohu_bullet_spawn(SDL_Point loc, float dx, float dy);
void twohu_bulletmanager_update(float *dt);
void twohu_bulletmanager_render(SDL_Renderer *renderer);
bool twohu_bulletmanager_is_colliding(TwohuEntity *entity);

#endif
