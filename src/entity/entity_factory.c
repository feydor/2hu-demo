#include "../constants.h"
#include "../util/util.h"
#include "entity_factory.h"
#include <stdlib.h>

TwohuEntity G_ENTITY_FACTORY[FACTORY_CAPACITY] = {0};
int factory_top = 0;

static void garbage_collect();

TwohuEntity *twohu_ef_create(TwohuEntityType type, FloatRect rect, SDL_Point hitbox) {
    if (factory_top >= FACTORY_CAPACITY) {
        garbage_collect();
    }

    TwohuEntity ent = {
        .type=type,
        .rect=rect,
        .hitbox=hitbox,
        .dx=0,
        .dy=0,
        .speed=DEFAULT_ENTITY_SPEED,
        .alive=true
    };

    switch (type) {
    case DEFAULT_ENEMY:
        ent.sheet_manager = twohu_ssm_create(DEFAULT_ENEMY_SPRITESHEET,
                                             DEFAULT_ENEMY_SPRITESHEET_NCOLS,
                                             DEFAULT_ENEMY_SPRITESHEET_NROWS);
        ent.speed = DEFAULT_ENEMY_SPEED; // TODO: randomize?
        ent.dy = DEFAULT_ENEMY_SPEED; // TODO: this entity should be responsive to events in twohu.c
        break;
    case PLAYER:
        ent.sheet_manager = twohu_ssm_create(PLAYER_SPRITESHEET,
                                             PLAYER_SPRITESHEET_NCOLS,
                                             PLAYER_SPRITESHEET_NROWS);
        ent.speed = DEFAULT_PLAYER_SPEED;
        break;
    default:
        // TODO: saner default?
        exit(fprintf(stderr, "twohu_ef_create: Unknown entity type encountered\n"));
    }

    G_ENTITY_FACTORY[factory_top++] = ent;
    return &G_ENTITY_FACTORY[factory_top - 1];
}

/** sort alive before !alive */
static int sort_by_alive_asc(const void * a, const void * b) {
    TwohuEntity *entity_a = (TwohuEntity *)a;
    TwohuEntity *entity_b = (TwohuEntity *)b;
    return (entity_b->alive - entity_a->alive);
}

/**
 * Moves alive entities to the front of the array and sets the new top at the first non-alive entity
 */
static void garbage_collect() {
    qsort(G_ENTITY_FACTORY, FACTORY_CAPACITY, sizeof(TwohuEntity), sort_by_alive_asc);

    int new_top = 0;
    while (G_ENTITY_FACTORY[new_top++].alive);

    factory_top = new_top;
}

void twohu_ef_update_all(float dt) {
    // randomly spawn an enemy every once in a while
    int n = rand() % 101;
    if (n < 20) {
        float rand_x = DEFAULT_ENEMY_SPRITE_W + (rand() % (W_WINDOW - DEFAULT_ENEMY_SPRITE_W));
        float rand_y = 1;
        twohu_ef_create(DEFAULT_ENEMY,
                        (FloatRect){.x=rand_x, .y=rand_y, .w=DEFAULT_ENEMY_SPRITE_W, .h=DEFAULT_ENEMY_SPRITE_H},
                        (SDL_Point){.x=DEFAULT_ENEMY_SPRITE_W - 5, .y=DEFAULT_ENEMY_SPRITE_H - 5});
    }

    // update all when alive
    for (int i=0; i<factory_top; ++i) {
        TwohuEntity *entity = &G_ENTITY_FACTORY[i];
        if (!entity->alive) continue;
        twohu_entity_update(entity, dt);
    }
}

void twohu_ef_render_all(SDL_Renderer *renderer) {
    for (int i=0; i<factory_top; ++i) {
        TwohuEntity *entity = &G_ENTITY_FACTORY[i];
        if (!entity->alive) continue;
        twohu_entity_render(entity, renderer);
    }
}