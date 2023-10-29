#include "bullet.h"
#include "../constants.h"
#include "../util/util.h"
TwohuBulletManager g_bullet_manager = {0};

/** The global circular buffer */
TwohuBullet g_bullets[MAX_BULLETS] = {0};
int g_top = 0;

/** global bullet surface */
SDL_Surface *g_bullet_surface;
SDL_Texture *g_bullet_texture;

TwohuBullet *twohu_bullet_spawn(SDL_Point loc, float dx, float dy) {
    if (!g_bullet_surface) {
        g_bullet_surface = load_surface_or_exit(BULLET_PNG);
    }

    TwohuBullet bullet = {
        .rect=(FloatRect){loc.x - (BULLET_W/2), loc.y - (BULLET_H/2), BULLET_W, BULLET_H},
        .hitbox=(SDL_Point){BULLET_W, BULLET_H},
        .surface=g_bullet_surface,
        .dx=dx,
        .dy=dy,
        .speed=BULLET_INIT_SPEED,
        .alive=true
    };
    
    if (g_top >= MAX_BULLETS) {
        printf("MAX_BULLETS reached! Resetting to 0!\n");
        g_top = 0;
    }

    g_bullets[g_top++] = bullet;
    return &g_bullets[g_top-1];
}

void twohu_bulletmanager_update(float dt) {
    for (int i=0; i<g_top; ++i) {
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

bool twohu_bulletmanager_is_colliding(TwohuEntity *entity) {
    for (int i=0; i<g_top; ++i) {
        TwohuBullet *bullet = &g_bullets[i];
        if (!bullet->alive) continue;

        if (floatrect_are_colliding(&bullet->rect, &entity->rect)) {
            return true;
        }
    }
    return false;
}

void twohu_bulletmanager_render(SDL_Renderer *renderer) {
    for (int i=0; i<g_top; ++i) {
        TwohuBullet *bullet = &g_bullets[i];
        if (!bullet->alive) continue;

        if (!g_bullet_texture) {
            g_bullet_texture = load_texture_or_exit(renderer, bullet->surface);
        }

        SDL_Rect render_rect = {.x=0, .y=0, .w=bullet->rect.w, .h=bullet->rect.h};
        SDL_Rect dest = floatrect_to_sdlrect(&bullet->rect);
        SDL_RenderCopy(renderer, g_bullet_texture, &render_rect, &dest);
    }
}
