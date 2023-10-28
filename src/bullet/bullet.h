#ifndef TWOHU_BULLET_H
#define TWOHU_BULLET_H
#include <SDL2/SDL.h>
#include <stdbool.h>

// typedef struct TwohuBulletManager TwohuBulletManager;
// typedef struct TwhohuBullet TwohuBullet;

typedef struct {
    SDL_Rect rect;
    SDL_Point hitbox;
    SDL_Surface *surface;
    float dx, dy;
    float speed;
    bool alive;
} TwohuBullet;

typedef struct {
    int top;
} TwohuBulletManager;

#define BULLET_PNG "./res/marisa-shot.png"
#define MAX_BULLETS 666 /* The size of the circular buffer */
#define BULLET_W 28
#define BULLET_H 62
#define BULLET_INIT_SPEED 3

/** retrieve the global bullet manager */
TwohuBulletManager twohu_bulletmanager_create();
TwohuBullet twohu_bullet_spawn(TwohuBulletManager *bm, SDL_Point loc, float dx, float dy);
void twohu_bulletmanager_update(TwohuBulletManager *bm, float dt);
void twohu_bulletmanager_render(TwohuBulletManager *bm, SDL_Renderer *renderer);

#endif
