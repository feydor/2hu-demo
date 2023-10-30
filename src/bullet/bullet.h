#ifndef TWOHU_BULLET_H
#define TWOHU_BULLET_H
#include <SDL2/SDL.h>
#include <stdbool.h>
#include "../util/rect.h"
#include "../entity/entity.h"

typedef struct TwohuImage {
    char *path;
    SDL_Surface *surface;
    SDL_Texture *texture;
} TwohuImage;

typedef struct {
    FloatRect rect;
    SDL_Point hitbox;
    TwohuImage *image;
    float dx, dy;
    float speed;
    bool alive;
    /** Whether or not this bullet can harm the given entity type */
    bool harmable[ENTITY_TYPE_COUNT];
} TwohuBullet;

#define MAX_BULLETS 666 /* The size of the circular buffer */
#define BULLET_INIT_SPEED 3

/** retrieve the global bullet manager */
TwohuBullet *twohu_bullet_spawn(TwohuEntityType entity_type, SDL_Point loc, float dx, float dy);
void twohu_bulletmanager_update(float *dt);
void twohu_bulletmanager_render(SDL_Renderer *renderer);
bool twohu_bulletmanager_is_colliding(TwohuEntity *entity);

#endif
