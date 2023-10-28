#include "enemy_manager.h"
#include "../constants.h"
#include <float.h>
#include <stdlib.h>
#include <SDL2/SDL_image.h>

/** global enemies circular buffer */
int g_enemies_top = 0;
TwohuEntity g_enemies[MAX_ENEMIES] = {0};

/** global default enemy surface */
SDL_Surface *g_enemy_surface;

/** global default enemy texture */
SDL_Texture *g_enemy_texture;

/** default enemy spawner */
static TwohuEntity *spawn_default_twohu_enemy();

TwohuEnemyManager create_twohu_enemymanager() {
    if (!g_enemy_surface) {
        g_enemy_surface = IMG_Load(DEFAULT_ENEMY_PNG);
        if (!g_enemy_surface) {
            exit(fprintf(stderr, "Failed to load the spritesheet: '%s'!\n", DEFAULT_ENEMY_PNG));
        }
    }

     return (TwohuEnemyManager) {
            .count = 0,
            .ticks = 0
    };
}

TwohuEntity *spawn_default_twohu_enemy() {
    float rand_x = DEFAULT_ENEMY_SPRITE_W + (rand() % (W_WINDOW - DEFAULT_ENEMY_SPRITE_W));
    float rand_y = 1;
    TwohuEntity enemy = create_twohu_enemy(
        (FloatRect){.x=rand_x, .y=rand_y, .w=DEFAULT_ENEMY_SPRITE_W, .h=DEFAULT_ENEMY_SPRITE_H},
        (SDL_Point){.x=DEFAULT_ENEMY_SPRITE_W - 5, .y=DEFAULT_ENEMY_SPRITE_H - 5}
    );
    enemy.surface = g_enemy_surface;
    enemy.alive = true;
    enemy.dy = DEFAULT_ENEMY_SPEED; // TODO: randomize

    if (g_enemies_top >= MAX_ENEMIES) {
        printf("MAX_ENEMIES reached! Resetting to 0!\n");
        g_enemies_top = 0;
    }

    g_enemies[g_enemies_top++] = enemy;
    return &g_enemies[g_enemies_top-1];
}

void twohu_enemymanager_update(TwohuEnemyManager *self, float dt) {
    self->ticks= self->ticks + dt >= FLT_MAX ? 0 : self->ticks + dt;

    // randomly spawn an enemy every once in a while
    int n = rand() % 101;
    if (n < 20) {
        spawn_default_twohu_enemy();
    }

    // update
    for (int i=0; i<g_enemies_top; ++i) {
        TwohuEntity *enemy = &g_enemies[i];
        if (!enemy->alive) continue;
        twohu_entity_update(enemy, dt);
    }
}

void twohu_enemymanager_render(TwohuEnemyManager *self, SDL_Renderer *renderer) {
    for (int i=0; i<g_enemies_top; ++i) {
        TwohuEntity *enemy = &g_enemies[i];
        if (!enemy->alive) continue;

        if (!g_enemy_texture) {
            g_enemy_texture = SDL_CreateTextureFromSurface(renderer, enemy->surface);
            if (!g_enemy_texture) {
                exit(fprintf(stderr, "Failed to create texture!\n"));
            }
        }

        SDL_Rect render_rect = {.x=0, .y=0, .w=enemy->rect.w, .h=enemy->rect.h};
        SDL_Rect dst_rect = floatrect_to_sdlrect(&enemy->rect);
        SDL_RenderCopy(renderer, g_enemy_texture, &render_rect, &dst_rect);
//        SDL_RenderFillRect(renderer, &dst_rect);
    }
}
