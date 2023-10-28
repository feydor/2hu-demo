#include "entity-factory.h"
#include "../util/rect.h"
#include "../util/util.h"
#include "../constants.h"

TwohuEntity G_ENTITY_FACTORY[FACTORY_CAPACITY] = {0};
int factory_top = 0;
SDL_Surface *g_default_entity_surface = NULL;


TwohuEntity *twohu_ef_create(TwohuEntityType type, TwohuSpritesheetManager *sm, FloatRect rect, SDL_Point hitbox) {
    if (factory_top >= FACTORY_CAPACITY) {
        // TODO: remalloc the pool
        exit(fprintf(stderr, "twohu_ef_create: Entity Factory reached FACTORY_CAPACITY=%d\n", FACTORY_CAPACITY));
    }

    if (!g_default_entity_surface) {
        g_default_entity_surface = SDL_CreateRGBSurface(0, rect.w, rect.h, 32, 0xFF000000, 0xFF0000, 0xFF00, 0xFF);
        if (!g_default_entity_surface) {
            exit(fprintf(stderr, "twohu_ef_create: Failed to load default entity surface\n"));
        }
    }

    TwohuEntity ent = {
        .type=type,
        .rect=rect,
        .hitbox=hitbox,
        .surface=g_default_entity_surface,
        .sheet_manager=create_twohu_spritesheet_manager(),
        .bullet_manager=twohu_bulletmanager_create(),
        .dx=0,
        .dy=0,
        .speed=DEFAULT_ENTITY_SPEED,
        .alive=true
    };

    G_ENTITY_FACTORY[factory_top++] = ent;
    return G_ENTITY_FACTORY[factory_top - 1];
}
