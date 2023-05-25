#include "bullet.h"
#include "constants.h"
#include <SDL2/SDL_image.h>

/* The global circular buffer */
TwohuBullet g_bullets[MAX_BULLETS];

TwohuBulletManager twohu_bulletmanager_create() {
    return (TwohuBulletManager){0};
}

TwohuBullet twohu_bullet_spawn(TwohuBulletManager *bm, SDL_Point loc, float dx, float dy) {
    SDL_Surface *image = IMG_Load(BULLET_PNG);
    if (!image) {
        exit(fprintf(stderr, "Failed to load the spritesheet: '%s'!\n", BULLET_PNG));
    }

    TwohuBullet bullet = {
        .rect=(SDL_Rect){loc.x - (BULLET_W/2), loc.y - (BULLET_H/2), BULLET_W, BULLET_H},
        .hitbox=(SDL_Point){BULLET_W, BULLET_H},
        .surface=image,
        .dx=dx,
        .dy=dy,
        .speed=BULLET_INIT_SPEED,
        .alive=true
    };
    
    if (bm->top >= MAX_BULLETS) {
        printf("MAX_BULLETS reached! Resetting to 0!\n");
        bm->top = 0;
    }

    g_bullets[bm->top++] = bullet;
    return bullet;
}

void twohu_bulletmanager_update(TwohuBulletManager *bm, float dt) {
    for (int i=0; i<bm->top; ++i) {
        TwohuBullet *b = &g_bullets[i];
        if (!b->alive) continue;

        float dx = b->dx * dt;
        float dy = b->dy * dt;
        float x_next = b->rect.x + dx;
        float y_next = b->rect.y + dy;

        // bounds checking
        if (y_next < 0 || y_next > H_WINDOW - b->rect.w ||
            x_next < 0 || x_next > W_WINDOW - b->rect.w) {
            b->alive = false;
            continue;
        }

        b->rect.x = x_next;
        b->rect.y = y_next;
    }
}

void twohu_bulletmanager_render(TwohuBulletManager *bm, SDL_Renderer *renderer) {
    // draw rect aka sprite for all bullets
    SDL_SetRenderDrawColor(renderer, 0xFF, 0, 0x00, 255);
    for (int i=0; i<bm->top; ++i) {
        TwohuBullet *b = &g_bullets[i];
        if (!b->alive) continue;

        SDL_Texture *txt = SDL_CreateTextureFromSurface(renderer, b->surface);
        if (!txt) {
            exit(fprintf(stderr, "Failed to create texture!\n"));
        }

        SDL_Rect render_rect = {.x=0, .y=0, .w=b->rect.w, .h=b->rect.h};
        SDL_RenderCopy(renderer, txt, &render_rect, &b->rect);
    }
}
